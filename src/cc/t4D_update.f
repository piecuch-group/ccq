       subroutine t4d_update(n0,n1,n2,n3,k1,k2,k3,k4,shift,
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
!       real*8,allocatable::t4a(:,:,:,:,:,:,:,:)
!       real*8 t4a(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1)
!       real*8,allocatable::t4b(:,:,:,:,:,:,:,:)
!       real*8 t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1)
       real*8,allocatable::t4c(:,:,:,:,:,:,:,:)
!       real*8 t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1)
       real*8,allocatable::t4d(:,:,:,:,:,:,:,:)
!       real*8 t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1)
       real*8,allocatable::t4e(:,:,:,:,:,:,:,:)
!       real*8 t4e(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2)
       real*8,allocatable::v4d(:,:,:,:,:,:,:,:)
!       real*8 v4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1)
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
       real*8,allocatable::q1(:,:)
       real*8,allocatable::q2(:,:)
       real*8,allocatable::s7(:,:,:,:)
       real*8,allocatable::q3(:,:)
       real*8,allocatable::s8(:,:,:,:)
       real*8,allocatable::q4(:,:)
       real*8,allocatable::s9(:,:,:,:)
       real*8,allocatable::u55(:,:,:,:,:,:)
       real*8,allocatable::s95(:,:,:,:)
       real*8,allocatable::s89(:,:,:,:)
       real*8,allocatable::s10(:,:,:,:)
       real*8,allocatable::u56(:,:,:,:,:,:)
       real*8,allocatable::s96(:,:,:,:)
       real*8,allocatable::s90(:,:,:,:)
       real*8,allocatable::s11(:,:,:,:)
       real*8,allocatable::u57(:,:,:,:,:,:)
       real*8,allocatable::s97(:,:,:,:)
       real*8,allocatable::q5(:,:)
       real*8,allocatable::s12(:,:,:,:)
       real*8,allocatable::u58(:,:,:,:,:,:)
       real*8,allocatable::s98(:,:,:,:)
       real*8,allocatable::q6(:,:)
       real*8,allocatable::s13(:,:,:,:)
       real*8,allocatable::u63(:,:,:,:,:,:)
       real*8,allocatable::s100(:,:,:,:)
       real*8,allocatable::s99(:,:,:,:)
       real*8,allocatable::s14(:,:,:,:)
       real*8,allocatable::u64(:,:,:,:,:,:)
       real*8,allocatable::s102(:,:,:,:)
       real*8,allocatable::s101(:,:,:,:)
       real*8,allocatable::s15(:,:,:,:)
       real*8,allocatable::s16(:,:,:,:)
       real*8,allocatable::s17(:,:,:,:)
       real*8,allocatable::s18(:,:,:,:)
       real*8,allocatable::s19(:,:,:,:)
       real*8,allocatable::s20(:,:,:,:)
       real*8,allocatable::s21(:,:,:,:)
       real*8,allocatable::s22(:,:,:,:)
       real*8,allocatable::s23(:,:,:,:)
       real*8,allocatable::s24(:,:,:,:)
       real*8,allocatable::s25(:,:,:,:)
       real*8,allocatable::u69(:,:,:,:,:,:)
       real*8,allocatable::s26(:,:,:,:)
       real*8,allocatable::u70(:,:,:,:,:,:)
       real*8,allocatable::q7(:,:)
       real*8,allocatable::q8(:,:)
       real*8,allocatable::s27(:,:,:,:)
       real*8,allocatable::u71(:,:,:,:,:,:)
       real*8,allocatable::s123(:,:,:,:)
       real*8,allocatable::s28(:,:,:,:)
       real*8,allocatable::u72(:,:,:,:,:,:)
       real*8,allocatable::q9(:,:)
       real*8,allocatable::s29(:,:,:,:)
       real*8,allocatable::u73(:,:,:,:,:,:)
       real*8,allocatable::s124(:,:,:,:)
       real*8,allocatable::s30(:,:,:,:)
       real*8,allocatable::u74(:,:,:,:,:,:)
       real*8,allocatable::q10(:,:)
       real*8,allocatable::s31(:,:,:,:)
       real*8,allocatable::u75(:,:,:,:,:,:)
       real*8,allocatable::s125(:,:,:,:)
       real*8,allocatable::s32(:,:,:,:)
       real*8,allocatable::u77(:,:,:,:,:,:)
       real*8,allocatable::u76(:,:,:,:,:,:)
       real*8,allocatable::s126(:,:,:,:)
       real*8,allocatable::q11(:,:)
       real*8,allocatable::s33(:,:,:,:)
       real*8,allocatable::u79(:,:,:,:,:,:)
       real*8,allocatable::u78(:,:,:,:,:,:)
       real*8,allocatable::s128(:,:,:,:)
       real*8,allocatable::s127(:,:,:,:)
       real*8,allocatable::s34(:,:,:,:)
       real*8,allocatable::u80(:,:,:,:,:,:)
       real*8,allocatable::q12(:,:)
       real*8,allocatable::s35(:,:,:,:)
       real*8,allocatable::s36(:,:,:,:)
       real*8,allocatable::s37(:,:,:,:)
       real*8,allocatable::q13(:,:)
       real*8,allocatable::q14(:,:)
       real*8,allocatable::s38(:,:,:,:)
       real*8,allocatable::u97(:,:,:,:,:,:)
       real*8,allocatable::s136(:,:,:,:)
       real*8,allocatable::s135(:,:,:,:)
       real*8,allocatable::u1(:,:,:,:,:,:)
       real*8,allocatable::u2(:,:,:,:,:,:)
       real*8,allocatable::u3(:,:,:,:,:,:)
       real*8,allocatable::u4(:,:,:,:,:,:)
       real*8,allocatable::u5(:,:,:,:,:,:)
       real*8,allocatable::u6(:,:,:,:,:,:)
       real*8,allocatable::u7(:,:,:,:,:,:)
       real*8,allocatable::u8(:,:,:,:,:,:)
       real*8,allocatable::u9(:,:,:,:,:,:)
       real*8,allocatable::s39(:,:,:,:)
       real*8,allocatable::s40(:,:,:,:)
       real*8,allocatable::u10(:,:,:,:,:,:)
       real*8,allocatable::s41(:,:,:,:)
       real*8,allocatable::u11(:,:,:,:,:,:)
       real*8,allocatable::s42(:,:,:,:)
       real*8,allocatable::u12(:,:,:,:,:,:)
       real*8,allocatable::u13(:,:,:,:,:,:)
       real*8,allocatable::s43(:,:,:,:)
       real*8,allocatable::u14(:,:,:,:,:,:)
       real*8,allocatable::u15(:,:,:,:,:,:)
       real*8,allocatable::s44(:,:,:,:)
       real*8,allocatable::u16(:,:,:,:,:,:)
       real*8,allocatable::s45(:,:,:,:)
       real*8,allocatable::s46(:,:,:,:)
       real*8,allocatable::u17(:,:,:,:,:,:)
       real*8,allocatable::s47(:,:,:,:)
       real*8,allocatable::s48(:,:,:,:)
       real*8,allocatable::s49(:,:,:,:)
       real*8,allocatable::s50(:,:,:,:)
       real*8,allocatable::u18(:,:,:,:,:,:)
       real*8,allocatable::s51(:,:,:,:)
       real*8,allocatable::s52(:,:,:,:)
       real*8,allocatable::u19(:,:,:,:,:,:)
       real*8,allocatable::s53(:,:,:,:)
       real*8,allocatable::s54(:,:,:,:)
       real*8,allocatable::u20(:,:,:,:,:,:)
       real*8,allocatable::u21(:,:,:,:,:,:)
       real*8,allocatable::s55(:,:,:,:)
       real*8,allocatable::u22(:,:,:,:,:,:)
       real*8,allocatable::u23(:,:,:,:,:,:)
       real*8,allocatable::s56(:,:,:,:)
       real*8,allocatable::u24(:,:,:,:,:,:)
       real*8,allocatable::u98(:,:,:,:,:,:)
       real*8,allocatable::s110(:,:,:,:)
       real*8,allocatable::u60(:,:,:,:,:,:)
       real*8,allocatable::u25(:,:,:,:,:,:)
       real*8,allocatable::s57(:,:,:,:)
       real*8,allocatable::s111(:,:,:,:)
       real*8,allocatable::s109(:,:,:,:)
       real*8,allocatable::u26(:,:,:,:,:,:)
       real*8,allocatable::u103(:,:,:,:,:,:)
       real*8,allocatable::u100(:,:,:,:,:,:)
       real*8,allocatable::u99(:,:,:,:,:,:)
       real*8,allocatable::s139(:,:,:,:)
       real*8,allocatable::u83(:,:,:,:,:,:)
       real*8,allocatable::u81(:,:,:,:,:,:)
       real*8,allocatable::u117(:,:,:,:,:,:)
       real*8,allocatable::u62(:,:,:,:,:,:)
       real*8,allocatable::u116(:,:,:,:,:,:)
       real*8,allocatable::u115(:,:,:,:,:,:)
       real*8,allocatable::u27(:,:,:,:,:,:)
       real*8,allocatable::u101(:,:,:,:,:,:)
       real*8,allocatable::u84(:,:,:,:,:,:)
       real*8,allocatable::s116(:,:,:,:)
       real*8,allocatable::s58(:,:,:,:)
       real*8,allocatable::u102(:,:,:,:,:,:)
       real*8,allocatable::s138(:,:,:,:)
       real*8,allocatable::s115(:,:,:,:)
       real*8,allocatable::u28(:,:,:,:,:,:)
       real*8,allocatable::s59(:,:,:,:)
       real*8,allocatable::u105(:,:,:,:,:,:)
       real*8,allocatable::s137(:,:,:,:)
       real*8,allocatable::s117(:,:,:,:)
       real*8,allocatable::s60(:,:,:,:)
       real*8,allocatable::q15(:,:)
       real*8,allocatable::u29(:,:,:,:,:,:)
       real*8,allocatable::s61(:,:,:,:)
       real*8,allocatable::u104(:,:,:,:,:,:)
       real*8,allocatable::s142(:,:,:,:)
       real*8,allocatable::s140(:,:,:,:)
       real*8,allocatable::s62(:,:,:,:)
       real*8,allocatable::s143(:,:,:,:)
       real*8,allocatable::s113(:,:,:,:)
       real*8,allocatable::q16(:,:)
       real*8,allocatable::s63(:,:,:,:)
       real*8,allocatable::s141(:,:,:,:)
       real*8,allocatable::s114(:,:,:,:)
       real*8,allocatable::q17(:,:)
       real*8,allocatable::q18(:,:)
       real*8,allocatable::u30(:,:,:,:,:,:)
       real*8,allocatable::u108(:,:,:,:,:,:)
       real*8,allocatable::u107(:,:,:,:,:,:)
       real*8,allocatable::u106(:,:,:,:,:,:)
       real*8,allocatable::s146(:,:,:,:)
       real*8,allocatable::u89(:,:,:,:,:,:)
       real*8,allocatable::u121(:,:,:,:,:,:)
       real*8,allocatable::u87(:,:,:,:,:,:)
       real*8,allocatable::u119(:,:,:,:,:,:)
       real*8,allocatable::u31(:,:,:,:,:,:)
       real*8,allocatable::s64(:,:,:,:)
       real*8,allocatable::u111(:,:,:,:,:,:)
       real*8,allocatable::s147(:,:,:,:)
       real*8,allocatable::s145(:,:,:,:)
       real*8,allocatable::u32(:,:,:,:,:,:)
       real*8,allocatable::u33(:,:,:,:,:,:)
       real*8,allocatable::u34(:,:,:,:,:,:)
       real*8,allocatable::s65(:,:,:,:)
       real*8,allocatable::s66(:,:,:,:)
       real*8,allocatable::u35(:,:,:,:,:,:)
       real*8,allocatable::u36(:,:,:,:,:,:)
       real*8,allocatable::s67(:,:,:,:)
       real*8,allocatable::u37(:,:,:,:,:,:)
       real*8,allocatable::u38(:,:,:,:,:,:)
       real*8,allocatable::s68(:,:,:,:)
       real*8,allocatable::u39(:,:,:,:,:,:)
       real*8,allocatable::u40(:,:,:,:,:,:)
       real*8,allocatable::s69(:,:,:,:)
       real*8,allocatable::s70(:,:,:,:)
       real*8,allocatable::u41(:,:,:,:,:,:)
       real*8,allocatable::s71(:,:,:,:)
       real*8,allocatable::u42(:,:,:,:,:,:)
       real*8,allocatable::s72(:,:,:,:)
       real*8,allocatable::u43(:,:,:,:,:,:)
       real*8,allocatable::u92(:,:,:,:,:,:)
       real*8,allocatable::s120(:,:,:,:)
       real*8,allocatable::u67(:,:,:,:,:,:)
       real*8,allocatable::u65(:,:,:,:,:,:)
       real*8,allocatable::u114(:,:,:,:,:,:)
       real*8,allocatable::u112(:,:,:,:,:,:)
       real*8,allocatable::u44(:,:,:,:,:,:)
       real*8,allocatable::s73(:,:,:,:)
       real*8,allocatable::s122(:,:,:,:)
       real*8,allocatable::s119(:,:,:,:)
       real*8,allocatable::u45(:,:,:,:,:,:)
       real*8,allocatable::u109(:,:,:,:,:,:)
       real*8,allocatable::s152(:,:,:,:)
       real*8,allocatable::u95(:,:,:,:,:,:)
       real*8,allocatable::u93(:,:,:,:,:,:)
       real*8,allocatable::u120(:,:,:,:,:,:)
       real*8,allocatable::s74(:,:,:,:)
       real*8,allocatable::u110(:,:,:,:,:,:)
       real*8,allocatable::s151(:,:,:,:)
       real*8,allocatable::u46(:,:,:,:,:,:)
       real*8,allocatable::s75(:,:,:,:)
       real*8,allocatable::s153(:,:,:,:)
       real*8,allocatable::s149(:,:,:,:)
       real*8,allocatable::q19(:,:)
       real*8,allocatable::s76(:,:,:,:)
       real*8,allocatable::s150(:,:,:,:)
       real*8,allocatable::q20(:,:)
       real*8,allocatable::u47(:,:,:,:,:,:)
       real*8,allocatable::s77(:,:,:,:)
       real*8,allocatable::s78(:,:,:,:)
       real*8,allocatable::u48(:,:,:,:,:,:)
       real*8,allocatable::u91(:,:,:,:,:,:)
       real*8,allocatable::s79(:,:,:,:)
       real*8,allocatable::s80(:,:,:,:)
       real*8,allocatable::s81(:,:,:,:)
       real*8,allocatable::s82(:,:,:,:)
       real*8,allocatable::u49(:,:,:,:,:,:)
       real*8,allocatable::u86(:,:,:,:,:,:)
       real*8,allocatable::u68(:,:,:,:,:,:)
       real*8,allocatable::s83(:,:,:,:)
       real*8,allocatable::u50(:,:,:,:,:,:)
       real*8,allocatable::u66(:,:,:,:,:,:)
       real*8,allocatable::u51(:,:,:,:,:,:)
       real*8,allocatable::s84(:,:,:,:)
       real*8,allocatable::s85(:,:,:,:)
       real*8,allocatable::u52(:,:,:,:,:,:)
       real*8,allocatable::u96(:,:,:,:,:,:)
       real*8,allocatable::s86(:,:,:,:)
       real*8,allocatable::u53(:,:,:,:,:,:)
       real*8,allocatable::u94(:,:,:,:,:,:)
       real*8,allocatable::s87(:,:,:,:)
       real*8,allocatable::u54(:,:,:,:,:,:)
       real*8,allocatable::u90(:,:,:,:,:,:)
       real*8,allocatable::s88(:,:,:,:)
       real*8,allocatable::s91(:,:,:,:)
       real*8,allocatable::u59(:,:,:,:,:,:)
       real*8,allocatable::q21(:,:)
       real*8,allocatable::s92(:,:,:,:)
       real*8,allocatable::q22(:,:)
       real*8,allocatable::s112(:,:,:,:)
       real*8,allocatable::q23(:,:)
       real*8,allocatable::s93(:,:,:,:)
       real*8,allocatable::u61(:,:,:,:,:,:)
       real*8,allocatable::q24(:,:)
       real*8,allocatable::s104(:,:,:,:)
       real*8,allocatable::s103(:,:,:,:)
       real*8,allocatable::u113(:,:,:,:,:,:)
       real*8,allocatable::s157(:,:,:,:)
       real*8,allocatable::s94(:,:,:,:)
       real*8,allocatable::s156(:,:,:,:)
       real*8,allocatable::s155(:,:,:,:)
       real*8,allocatable::s105(:,:,:,:)
       real*8,allocatable::u85(:,:,:,:,:,:)
       real*8,allocatable::u82(:,:,:,:,:,:)
       real*8,allocatable::s129(:,:,:,:)
       real*8,allocatable::q25(:,:)
       real*8,allocatable::s106(:,:,:,:)
       real*8,allocatable::s158(:,:,:,:)
       real*8,allocatable::s107(:,:,:,:)
       real*8,allocatable::q28(:,:)
       real*8,allocatable::s108(:,:,:,:)
       real*8,allocatable::q26(:,:)
       real*8,allocatable::s121(:,:,:,:)
       real*8,allocatable::s118(:,:,:,:)
       real*8,allocatable::q27(:,:)
       real*8,allocatable::s130(:,:,:,:)
       real*8,allocatable::u88(:,:,:,:,:,:)
       real*8,allocatable::q29(:,:)
       real*8,allocatable::s132(:,:,:,:)
       real*8,allocatable::s160(:,:,:,:)
       real*8,allocatable::s131(:,:,:,:)
       real*8,allocatable::u118(:,:,:,:,:,:)
       real*8,allocatable::s159(:,:,:,:)
       real*8,allocatable::s133(:,:,:,:)
       real*8,allocatable::q30(:,:)
       real*8,allocatable::s134(:,:,:,:)
       real*8,allocatable::q31(:,:)
       real*8,allocatable::s144(:,:,:,:)
       real*8,allocatable::q32(:,:)
       real*8,allocatable::s154(:,:,:,:)
       real*8,allocatable::s148(:,:,:,:)
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
       real*8,allocatable::x21(:,:,:,:,:,:)
       real*8,allocatable::z205(:,:,:,:,:,:,:,:)
       real*8,allocatable::x22(:,:,:,:,:,:)
       real*8,allocatable::z206(:,:,:,:,:,:,:,:)
       real*8,allocatable::x23(:,:,:,:,:,:)
       real*8,allocatable::z208(:,:,:,:,:,:,:,:)
       real*8,allocatable::x24(:,:,:,:,:,:)
       real*8,allocatable::z223(:,:,:,:,:,:,:,:)
       real*8,allocatable::x25(:,:,:,:)
       real*8,allocatable::z46(:,:,:,:,:,:,:,:)
       real*8,allocatable::x26(:,:,:,:)
       real*8,allocatable::z47(:,:,:,:,:,:,:,:)
       real*8,allocatable::x27(:,:,:,:,:,:)
       real*8,allocatable::z247(:,:,:,:,:,:,:,:)
       real*8,allocatable::z61(:,:,:,:,:,:,:,:)
       real*8,allocatable::x28(:,:,:,:,:,:)
       real*8,allocatable::z255(:,:,:,:,:,:,:,:)
       real*8,allocatable::z65(:,:,:,:,:,:,:,:)
       real*8,allocatable::x29(:,:,:,:,:,:)
       real*8,allocatable::z81(:,:,:,:,:,:,:,:)
       real*8,allocatable::x30(:,:,:,:,:,:)
       real*8,allocatable::z84(:,:,:,:,:,:,:,:)
       real*8,allocatable::x31(:,:,:,:,:,:)
       real*8,allocatable::z86(:,:,:,:,:,:,:,:)
       real*8,allocatable::x32(:,:,:,:,:,:)
       real*8,allocatable::z88(:,:,:,:,:,:,:,:)
       real*8,allocatable::x33(:,:,:,:,:,:)
       real*8,allocatable::z89(:,:,:,:,:,:,:,:)
       real*8,allocatable::x34(:,:,:,:,:,:)
       real*8,allocatable::z91(:,:,:,:,:,:,:,:)
       real*8,allocatable::x35(:,:,:,:,:,:)
       real*8,allocatable::z94(:,:,:,:,:,:,:,:)
       real*8,allocatable::x36(:,:,:,:,:,:)
       real*8,allocatable::z105(:,:,:,:,:,:,:,:)
       real*8,allocatable::x37(:,:,:,:,:,:)
       real*8,allocatable::z108(:,:,:,:,:,:,:,:)
       real*8,allocatable::x38(:,:,:,:,:,:)
       real*8,allocatable::z112(:,:,:,:,:,:,:,:)
       real*8,allocatable::z114(:,:,:,:,:,:,:,:)
       real*8,allocatable::z117(:,:,:,:,:,:,:,:)
       real*8,allocatable::z118(:,:,:,:,:,:,:,:)
       real*8,allocatable::x39(:,:,:,:,:,:)
       real*8,allocatable::z120(:,:,:,:,:,:,:,:)
       real*8,allocatable::z304(:,:,:,:,:,:,:,:)
       real*8,allocatable::z303(:,:,:,:,:,:,:,:)
       real*8,allocatable::z302(:,:,:,:,:,:,:,:)
       real*8,allocatable::z131(:,:,:,:,:,:,:,:)
       real*8,allocatable::x40(:,:,:,:,:,:)
       real*8,allocatable::z135(:,:,:,:,:,:,:,:)
       real*8,allocatable::x41(:,:,:,:,:,:)
       real*8,allocatable::z136(:,:,:,:,:,:,:,:)
       real*8,allocatable::x42(:,:,:,:,:,:)
       real*8,allocatable::z139(:,:,:,:,:,:,:,:)
       real*8,allocatable::x43(:,:,:,:,:,:)
       real*8,allocatable::z140(:,:,:,:,:,:,:,:)
       real*8,allocatable::x44(:,:,:,:,:,:)
       real*8,allocatable::z142(:,:,:,:,:,:,:,:)
       real*8,allocatable::x45(:,:,:,:,:,:)
       real*8,allocatable::z143(:,:,:,:,:,:,:,:)
       real*8,allocatable::x46(:,:,:,:,:,:)
       real*8,allocatable::z145(:,:,:,:,:,:,:,:)
       real*8,allocatable::x47(:,:,:,:,:,:)
       real*8,allocatable::z146(:,:,:,:,:,:,:,:)
       real*8,allocatable::x48(:,:,:,:,:,:)
       real*8,allocatable::z149(:,:,:,:,:,:,:,:)
       real*8,allocatable::z151(:,:,:,:,:,:,:,:)
       real*8,allocatable::z153(:,:,:,:,:,:,:,:)
       real*8,allocatable::z305(:,:,:,:,:,:,:,:)
       real*8,allocatable::z156(:,:,:,:,:,:,:,:)
       real*8,allocatable::z169(:,:,:,:,:,:,:,:)
       real*8,allocatable::z170(:,:,:,:,:,:,:,:)
       real*8,allocatable::z172(:,:,:,:,:,:,:,:)
       real*8,allocatable::z173(:,:,:,:,:,:,:,:)
       real*8,allocatable::z176(:,:,:,:,:,:,:,:)
       real*8,allocatable::z290(:,:,:,:,:,:,:,:)
       real*8,allocatable::z177(:,:,:,:,:,:,:,:)
       real*8,allocatable::z178(:,:,:,:,:,:,:,:)
       real*8,allocatable::z181(:,:,:,:,:,:,:,:)
c
!       allocate(t4a(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
!       allocate(t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       allocate(t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       allocate(t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       allocate(t4e(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       allocate(v4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
c
!       rewind(ta)
!       rewind(tb)
       rewind(tc)
       rewind(td)
       rewind(te)
!       read(ta)t4a
!       read(tb)t4b
       read(tc)t4c
       read(td)t4d
       read(te)t4e
c
       allocate(indocc(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       allocate(indunocc(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       indocc=0
       indunocc=0
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        iocca=0
        ioccb=0
        if(i.gt.(n1-iactocca))iocca=iocca+1
        if(j.gt.(n2-iactoccb))ioccb=ioccb+1
        if(k.gt.(n2-iactoccb))ioccb=ioccb+1
        if(l.gt.(n2-iactoccb))ioccb=ioccb+1
        if(iocca+ioccb.lt.iactindq)indocc(l,k,j,i)=1
       enddo;enddo;enddo;enddo
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           iunoa=0
           iunob=0
           if(a.lt.(n1+iactunoa+1))iunoa=iunoa+1
           if(b.lt.(n2+iactunob+1))iunob=iunob+1
           if(c.lt.(n2+iactunob+1))iunob=iunob+1
           if(d.lt.(n2+iactunob+1))iunob=iunob+1
           if(iunoa+iunob.lt.iactindq)indunocc(d,c,b,a)=1
          enddo;enddo;enddo;enddo
c
       v4d=0.0d0
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
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n0,n2,n0,n1,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s3(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s3)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x5(n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
       x5=0.0d0
       call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x5,s3,-1.000)
       deallocate(s3)
c
       allocate(d1(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       call reorder1423(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n2,n3,n0,n1,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s4(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s4)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x6(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       x6=0.0d0
       call sum3124(n2,n3,n2,n3,n1,n3,n0,n1,x6,s4,-1.000)
       deallocate(s4)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n1,n3,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s5(n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s5)
       deallocate(d1)
       deallocate(b2)
c
       call sum4123(n0,n2,n1,n3,n0,n2,n0,n1,x5,s5, 1.000)
       deallocate(s5)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s6(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s6)
       deallocate(d1)
       deallocate(b2)
c
       call sum4123(n2,n3,n2,n3,n1,n3,n0,n1,x6,s6, 1.000)
       deallocate(s6)
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
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s7(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s7)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x12(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       x12=0.0d0
       call sum3241(n0,n1,n1,n3,n1,n3,n0,n1,x12,s7,-1.000)
       deallocate(s7)
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
       allocate(s8(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s8)
       deallocate(d1)
       deallocate(b2)
c
       call sum4231(n0,n1,n1,n3,n1,n3,n0,n1,x12,s8, 1.000)
       deallocate(s8)
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
       allocate(s9(n0+1:n1,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s9)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x13(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       x13=0.0d0
       call sum4231(n0,n2,n0,n1,n0,n2,n0,n1,x13,s9, 1.000)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder2431(n0,n1,n0,n1,n0,n2,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s9,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u55(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u55)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x21(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x21=0.0d0
       call sum234156(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,u55,1.000)
       deallocate(u55)
c
       allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       call reorder4231(n0,n1,n0,n1,n0,n2,n0,n2,
     & n0,n2,n0,n1,n0,n2,n0,n1,s9,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s95(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s95)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s95,-1.000)
       deallocate(s95)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder2431(n0,n1,n0,n1,n0,n2,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s9,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s89(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s89)
       deallocate(d1)
       deallocate(b2)
       deallocate(s9)
c
       call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x5,s89,-1.000)
       deallocate(s89)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n2,n3,n0,n1,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s10(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s10)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x14(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       x14=0.0d0
       call sum4231(n0,n1,n2,n3,n2,n3,n0,n1,x14,s10, 1.000)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s10,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u56(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u56)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x22(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x22=0.0d0
       call sum345126(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x22,u56,1.000)
       deallocate(u56)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s10,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s96(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s96)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n2,n3,n0,n2,n0,n1,x1,s96, 1.000)
       deallocate(s96)
c
       allocate(d1(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       call reorder4231(n0,n1,n2,n3,n2,n3,n0,n1,
     & n0,n1,n2,n3,n2,n3,n0,n1,s10,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s90(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s90)
       deallocate(d1)
       deallocate(b2)
       deallocate(s10)
c
       call sum3124(n2,n3,n2,n3,n1,n3,n0,n1,x6,s90,-1.000)
       deallocate(s90)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s11(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s11)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x15(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       x15=0.0d0
       call sum3241(n0,n2,n1,n3,n1,n3,n0,n2,x15,s11,-1.000)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder2413(n1,n3,n1,n3,n0,n2,n0,n2,
     & n1,n3,n0,n2,n1,n3,n0,n2,s11,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u57(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u57)
       deallocate(d1)
       deallocate(d2)
c
       call sum246135(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,u57,1.000)
       deallocate(u57)
c
       allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4213(n1,n3,n1,n3,n0,n2,n0,n2,
     & n0,n2,n1,n3,n1,n3,n0,n2,s11,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s97(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s97)
       deallocate(d1)
       deallocate(b2)
       deallocate(s11)
c
       call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s97, 1.000)
       deallocate(s97)
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
       allocate(s12(n1+1:n3,n2+1:n3,n1+1:n3,n2+1:n3))
       i1=k4*k3*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s12)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x16(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       x16=0.0d0
       call sum4123(n2,n3,n1,n3,n2,n3,n1,n3,x16,s12,-1.000)
c
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       call reorder3241(n1,n3,n2,n3,n1,n3,n2,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,s12,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u58(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k4
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u58)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x23(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       x23=0.0d0
       call sum356124(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x23,u58,1.000)
       deallocate(u58)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       call reorder2341(n1,n3,n2,n3,n1,n3,n2,n3,
     & n2,n3,n1,n3,n2,n3,n1,n3,s12,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s98(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s98)
       deallocate(d1)
       deallocate(b2)
       deallocate(s12)
c
       call sum4123(n1,n3,n2,n3,n1,n3,n0,n2,x2,s98,-1.000)
       deallocate(s98)
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
       allocate(s13(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s13)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x20(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       x20=0.0d0
       call sum3241(n0,n2,n2,n3,n1,n3,n0,n1,x20,s13,-1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2413(n1,n3,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s13,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u63(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u63)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x24(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x24=0.0d0
       call sum245136(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x24,u63,1.000)
       deallocate(u63)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder4213(n1,n3,n2,n3,n0,n1,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n1,s13,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s100(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s100)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s100, 1.000)
       deallocate(s100)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2413(n1,n3,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s13,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s99(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s99)
       deallocate(d1)
       deallocate(b2)
       deallocate(s13)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s99,-1.000)
       deallocate(s99)
c
       allocate(d1(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s14(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s14)
       deallocate(d1)
       deallocate(b2)
c
       call sum4231(n0,n2,n2,n3,n1,n3,n0,n1,x20,s14, 1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n1,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s14,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u64(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u64)
       deallocate(d1)
       deallocate(d2)
c
      call sum245136(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x24,u64,-1.000)
       deallocate(u64)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n1,n2,n3,n1,n3,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n1,s14,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s102(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s102)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s102,-1.000)
       deallocate(s102)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n1,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s14,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s101(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s101)
       deallocate(d1)
       deallocate(b2)
       deallocate(s14)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s101, 1.000)
       deallocate(s101)
c
       allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n0,n2,n0,n1,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s15(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s15)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s15,-1.000)
       deallocate(s15)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n2,n3,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s16(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s16)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n2,n3,n0,n2,n0,n1,x1,s16, 1.000)
       deallocate(s16)
c
       allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder2314(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n1,n3,n1,n3,n0,n2,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s17(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s17)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s17,-1.000)
       deallocate(s17)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n1,n3,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s18(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s18)
       deallocate(d1)
       deallocate(b2)
c
       call sum4123(n1,n3,n2,n3,n1,n3,n0,n2,x2,s18, 1.000)
       deallocate(s18)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s19(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s19)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x3(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       x3=0.0d0
       call sum2134(n0,n2,n2,n3,n0,n2,n0,n2,x3,s19,-1.000)
       deallocate(s19)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s20(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s20)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x25(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       x25=0.0d0
       call sum3124(n0,n2,n2,n3,n0,n2,n0,n2,x25,s20, 1.000)
       deallocate(s20)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s21(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s21)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x26(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       x26=0.0d0
       call sum3124(n2,n3,n2,n3,n2,n3,n0,n2,x26,s21, 1.000)
       deallocate(s21)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n2,n3,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s22(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4*k4
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s22)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x4(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       x4=0.0d0
       call sum4123(n2,n3,n2,n3,n2,n3,n0,n2,x4,s22, 1.000)
       deallocate(s22)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s23(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s23)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s23, 1.000)
       deallocate(s23)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s24(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s24)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s24,-1.000)
       deallocate(s24)
c
       allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n1,n3,n0,n2,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s25(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s25)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x7(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       x7=0.0d0
       call sum3124(n0,n1,n1,n3,n2,n3,n0,n2,x7,s25,-1.000)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3214(n2,n3,n0,n1,n1,n3,n0,n2,
     & n1,n3,n0,n1,n2,n3,n0,n2,s25,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u69(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u69)
       deallocate(d1)
       deallocate(d2)
       deallocate(s25)
c
       allocate(x27(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x27=0.0d0
       call sum346125(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x27,u69,1.000)
       deallocate(u69)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s26(n0+1:n2,n1+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s26)
       deallocate(d1)
       deallocate(b2)
c
       call sum4231(n0,n1,n1,n3,n2,n3,n0,n2,x7,s26, 1.000)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2431(n0,n2,n1,n3,n2,n3,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,s26,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u70(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u70)
       deallocate(d1)
       deallocate(d2)
       deallocate(s26)
c
      call sum346125(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x27,u70,-1.000)
       deallocate(u70)
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
       allocate(s27(n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s27)
       deallocate(d1)
       deallocate(b2)
c
       call sum3241(n0,n2,n0,n1,n0,n2,n0,n1,x13,s27, 1.000)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder2413(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s27,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u71(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u71)
       deallocate(d1)
       deallocate(d2)
c
       call sum234156(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,u71,1.000)
       deallocate(u71)
c
       allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       call reorder4213(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n2,n0,n1,n0,n2,n0,n1,s27,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s123(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s123)
       deallocate(d1)
       deallocate(b2)
       deallocate(s27)
c
       call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s123,-1.000)
       deallocate(s123)
c
       allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n0,n1,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s28(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s28)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n2,n3,n2,n3,n0,n1,x14,s28,-1.000)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder3214(n2,n3,n0,n1,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s28,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u72(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u72)
       deallocate(d1)
       deallocate(d2)
       deallocate(s28)
c
      call sum345126(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x22,u72,-1.000)
       deallocate(u72)
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
       allocate(s29(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s29)
       deallocate(d1)
       deallocate(b2)
c
       call sum4231(n0,n2,n1,n3,n1,n3,n0,n2,x15,s29, 1.000)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder2431(n0,n2,n1,n3,n1,n3,n0,n2,
     & n1,n3,n0,n2,n1,n3,n0,n2,s29,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u73(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u73)
       deallocate(d1)
       deallocate(d2)
c
      call sum246135(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,u73,-1.000)
       deallocate(u73)
c
       allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4231(n0,n2,n1,n3,n1,n3,n0,n2,
     & n0,n2,n1,n3,n1,n3,n0,n2,s29,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s124(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s124)
       deallocate(d1)
       deallocate(b2)
       deallocate(s29)
c
       call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s124,-1.000)
       deallocate(s124)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s30(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s30)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n2,n3,n1,n3,n2,n3,n1,n3,x16,s30,-1.000)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       call reorder3214(n2,n3,n2,n3,n1,n3,n1,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,s30,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u74(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k4
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u74)
       deallocate(d1)
       deallocate(d2)
       deallocate(s30)
c
       call sum356124(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x23,u74,1.000)
       deallocate(u74)
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
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s31(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s31)
       deallocate(d1)
       deallocate(b2)
c
!       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
!       call reorder4213(n0,n2,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n0,n2,n0,n2,n0,n2,s31,d1)
!       allocate(h2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       call reorder56123478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t4d,h2)
!       allocate(z61(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2
!       i2=k1*k2*k3*k4*k4*k4
!       i3=k2*k2
!       call egemm(i1,i2,i3,d1,h2,z61)
!       deallocate(d1)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2;do n=n0+1,n2
             sum=sum
     &     + (s31(k,m,j,n)*t4d(d,c,b,a,n,m,l,i)       !kmjndcbanmli    (+0.500)
     &     - s31(l,m,j,n)*t4d(d,c,b,a,n,m,k,i)        !lmjndcbanmki    (-0.500)
     &     - s31(j,m,k,n)*t4d(d,c,b,a,n,m,l,i)        !jmkndcbanmli    (-0.500)
     &     + s31(j,m,l,n)*t4d(d,c,b,a,n,m,k,i)        !jmlndcbanmki    (+0.500)
     &     + s31(l,m,k,n)*t4d(d,c,b,a,n,m,j,i)        !lmkndcbanmji    (+0.500)
     &     - s31(k,m,l,n)*t4d(d,c,b,a,n,m,j,i))/2.0d0 !kmlndcbanmji    (-0.500)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12345867(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z61, 0.500)
!       call sum12346857(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z61,-0.500)
!       call sum12345876(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z61,-0.500)
!       call sum12346875(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z61, 0.500)
!       call sum12347856(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z61, 0.500)
!       call sum12347865(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z61,-0.500)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z61(d,c,b,a,l,i,k,j)      ! 12345867 (+0.500)
!     & -z61(d,c,b,a,k,i,l,j)       ! 12346857 (-0.500)
!     & -z61(d,c,b,a,l,i,j,k)       ! 12345876 (-0.500)
!     & +z61(d,c,b,a,k,i,j,l)       ! 12346875 (+0.500)
!     & +z61(d,c,b,a,j,i,l,k)       ! 12347856 (+0.500)
!     & -z61(d,c,b,a,j,i,k,l))/2.0d0! 12347865 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z61)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder2413(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s31,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(u75(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,d1,d2,u75)
       deallocate(d1)
       deallocate(d2)
c
      call sum236145(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,u75,-1.000)
       deallocate(u75)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder2413(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s31,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s125(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s125)
       deallocate(d1)
       deallocate(b2)
       deallocate(s31)
c
       call sum2134(n0,n2,n2,n3,n0,n2,n0,n2,x25,s125,-1.000)
       deallocate(s125)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s32(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s32)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x18(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       x18=0.0d0
       call sum3241(n0,n2,n2,n3,n2,n3,n0,n2,x18,s32,-1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,s32,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u77(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u77)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x28(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       x28=0.0d0
       call sum345126(n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,x28,u77,1.000)
       deallocate(u77)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,s32,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u76(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u76)
       deallocate(d1)
       deallocate(d2)
c
      call sum346125(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,u76,-1.000)
       deallocate(u76)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,s32,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s126(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s126)
       deallocate(d1)
       deallocate(b2)
       deallocate(s32)
c
       call sum2134(n2,n3,n2,n3,n2,n3,n0,n2,x4,s126, 1.000)
       deallocate(s126)
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
       allocate(s33(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s33)
       deallocate(d1)
       deallocate(b2)
c
       call sum4231(n0,n2,n2,n3,n2,n3,n0,n2,x18,s33, 1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder2431(n0,n2,n2,n3,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,s33,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u79(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u79)
       deallocate(d1)
       deallocate(d2)
c
      call sum345126(n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,x28,u79,-1.000)
       deallocate(u79)
c
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder2431(n0,n2,n2,n3,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,s33,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u78(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u78)
       deallocate(d1)
       deallocate(d2)
c
       call sum346125(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,u78,1.000)
       deallocate(u78)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder4231(n0,n2,n2,n3,n2,n3,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,s33,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s128(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s128)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n2,n3,n2,n3,n2,n3,n0,n2,x26,s128, 1.000)
       deallocate(s128)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder2431(n0,n2,n2,n3,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,s33,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s127(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s127)
       deallocate(d1)
       deallocate(b2)
       deallocate(s33)
c
       call sum3124(n0,n2,n2,n3,n0,n2,n0,n2,x3,s127, 1.000)
       deallocate(s127)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s34(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s34)
       deallocate(d1)
       deallocate(b2)
c
!       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
!       call reorder2341(n2,n3,n2,n3,n2,n3,n2,n3,
!     & n2,n3,n2,n3,n2,n3,n2,n3,s34,d1)
!       allocate(h2(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       call reorder12345678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,t4d,h2)
!       allocate(z65(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1,
!     & n2+1:n3,n2+1:n3))
!       i1=k4*k4
!       i2=k1*k2*k2*k2*k3*k4
!       i3=k4*k4
!       call egemm(i1,i2,i3,d1,h2,z65)
!       deallocate(d1)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do f=n2+1,n3
             sum=sum
     &     + (s34(c,f,e,d)*t4d(f,e,b,a,l,k,j,i)       !cfedfebalkji    (+0.500)
     &     - s34(b,f,e,d)*t4d(f,e,c,a,l,k,j,i)        !bfedfecalkji    (-0.500)
     &     - s34(d,f,e,c)*t4d(f,e,b,a,l,k,j,i)        !dfecfebalkji    (-0.500)
     &     + s34(d,f,e,b)*t4d(f,e,c,a,l,k,j,i)        !dfebfecalkji    (+0.500)
     &     + s34(b,f,e,c)*t4d(f,e,d,a,l,k,j,i)        !bfecfedalkji    (+0.500)
     &     - s34(c,f,e,b)*t4d(f,e,d,a,l,k,j,i))/2.0d0 !cfebfedalkji    (-0.500)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34567812(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z65, 0.500)
!       call sum24567813(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z65,-0.500)
!       call sum34567821(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z65,-0.500)
!       call sum24567831(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z65, 0.500)
!       call sum14567823(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z65, 0.500)
!       call sum14567832(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z65,-0.500)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z65(b,a,l,k,j,i,d,c)      ! 34567812 (+0.500)
!     & -z65(c,a,l,k,j,i,d,b)       ! 24567813 (-0.500)
!     & -z65(b,a,l,k,j,i,c,d)       ! 34567821 (-0.500)
!     & +z65(c,a,l,k,j,i,b,d)       ! 24567831 (+0.500)
!     & +z65(d,a,l,k,j,i,c,b)       ! 14567823 (+0.500)
!     & -z65(d,a,l,k,j,i,b,c))/2.0d0! 14567832 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z65)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder3241(n2,n3,n2,n3,n2,n3,n2,n3,
     & n2,n3,n2,n3,n2,n3,n2,n3,s34,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u80(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4*k4
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u80)
       deallocate(d1)
       deallocate(d2)
       deallocate(s34)
c
      call sum456123(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x23,u80,-1.000)
       deallocate(u80)
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
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s35(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s35)
       deallocate(d1)
       deallocate(d2)
c
       call sum2431(n0,n2,n1,n3,n0,n2,n0,n1,x5,s35, 1.000)
       deallocate(s35)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n2,n3,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1324(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,t2a,d2)
       allocate(s36(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k1*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s36)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n2,n3,n2,n3,n1,n3,n0,n1,x6,s36, 1.000)
       deallocate(s36)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1324(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,t2a,d2)
       allocate(s37(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s37)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n1,n3,n0,n1,x12,s37, 1.000)
       deallocate(s37)
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
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1324(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,t2a,d2)
       allocate(s38(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s38)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n1,n3,n0,n1,x20,s38, 1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s38,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u97(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u97)
       deallocate(d1)
       deallocate(d2)
c
      call sum245136(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x24,u97,-1.000)
       deallocate(u97)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n1,n3,n0,n1,n2,n3,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n1,s38,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s136(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s136)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s136,-1.000)
       deallocate(s136)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s38,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s135(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s135)
       deallocate(d1)
       deallocate(b2)
       deallocate(s38)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s135, 1.000)
       deallocate(s135)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u1)
       deallocate(d1)
       deallocate(d2)
c
       call sum346125(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x27,u1,-1.000)
       deallocate(u1)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n0,n2,n0,n1,intm,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u2)
       deallocate(d1)
       deallocate(d2)
c
       call sum234156(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,u2, 1.000)
       deallocate(u2)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n2,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u3(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u3)
       deallocate(d1)
       deallocate(d2)
c
       call sum345126(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x22,u3, 1.000)
       deallocate(u3)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n1,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u4(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u4)
       deallocate(d1)
       deallocate(d2)
c
       call sum246135(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,u4,-1.000)
       deallocate(u4)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,intm,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u5(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k4
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u5)
       deallocate(d1)
       deallocate(d2)
c
       call sum356124(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x23,u5,-1.000)
       deallocate(u5)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(u6(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,d1,d2,u6)
       deallocate(d1)
       deallocate(d2)
c
       call sum236145(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x24,u6,1.000)
       deallocate(u6)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u7(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u7)
       deallocate(d1)
       deallocate(d2)
c
       call sum346125(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,u7,1.000)
       deallocate(u7)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u8(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u8)
       deallocate(d1)
       deallocate(d2)
c
       call sum345126(n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,x28,u8,-1.000)
       deallocate(u8)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n2,n3,intb,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u9(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4*k4
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u9)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x29(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       x29=0.0d0
       call sum456123(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x29,u9,1.000)
       deallocate(u9)
c
       allocate(b1(n1+1:n3,n0+1:n1))
       call reorder12(0,n3,0,n3,
     & n1,n3,n0,n1,fockr,b1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s39(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,b1,d2,s39)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s39, 1.000)
       deallocate(s39)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n1,n1,n3,fockr,b1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(s40(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
       i1=k3
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s40)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x2,s40,-1.000)
       deallocate(s40)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder361245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
       allocate(u10(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k2*k4*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u10)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x30(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x30=0.0d0
       call sum234516(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x30,u10,1.000)
       deallocate(u10)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(s41(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s41)
       deallocate(d1)
       deallocate(d2)
c
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s41,-1.000)
       deallocate(s41)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u11(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u11)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x31(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       x31=0.0d0
       call sum356241(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,x31,u11,1.000)
       deallocate(u11)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,t2b,d2)
       allocate(s42(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k2*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s42)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n1,n3,n2,n3,n1,n3,n0,n2,x2,s42,-1.000)
       deallocate(s42)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n2,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u12(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u12)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x32(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x32=0.0d0
       call sum346251(n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,x32,u12,1.000)
       deallocate(u12)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder631245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
       allocate(u13(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k2*k4*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,f2,u13)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x33(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       x33=0.0d0
       call sum234561(n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,x33,u13,1.000)
       deallocate(u13)
c
       allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n0,n1,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder2314(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n2,n2,n3,n0,n1,t2b,d2)
       allocate(s43(n2+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,d2,s43)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n0,n1,n2,n3,n0,n2,n0,n1,x1,s43,-1.000)
       deallocate(s43)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n2,n3,n0,n1,intm,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u14(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k4
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u14)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x34(n0+1:n1,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       x34=0.0d0
       call sum456231(n0,n1,n2,n3,n2,n3,n2,n3,n0,n2,n0,n1,x34,u14,1.000)
       deallocate(u14)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u15(n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k3
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u15)
       deallocate(d1)
       deallocate(d2)
c
       call sum456231(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,x31,u15,1.000)
       deallocate(u15)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s44(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4
       i2=k1*k2
       i3=k3*k4
       call egemm(i1,i2,i3,d1,d2,s44)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n2,n3,n0,n2,n0,n1,x1,s44, 1.000)
       deallocate(s44)
c
       allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n0,n1,n0,n2,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder341256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n1,n3,n0,n2,n2,n3,n2,n3,n0,n2,n0,n1,t3c,f2)
       allocate(u16(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k2*k4*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,f2,u16)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x35(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x35=0.0d0
       call sum234615(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x35,u16,1.000)
       deallocate(u16)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(s45(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s45)
       deallocate(d1)
       deallocate(d2)
c
       call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x25,s45,-1.000)
       deallocate(s45)
c
       allocate(d1(n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder1234(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n1,n3,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder4312(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n0,n2,n2,n3,n1,n3,t2b,d2)
       allocate(s46(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k3*k4
       i3=k2*k1
       call egemm(i1,i2,i3,d1,d2,s46)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n1,n3,n2,n3,n1,n3,n0,n2,x2,s46, 1.000)
       deallocate(s46)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder132456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n1,n3,n2,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
       allocate(u17(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4
       i2=k1*k2*k2*k4
       i3=k3*k4
       call egemm(i1,i2,i3,d1,f2,u17)
       deallocate(d1)
       deallocate(f2)
c
       call sum345621(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x22,u17,1.000)
       deallocate(u17)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n2,n3,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,t2b,d2)
       allocate(s47(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k2*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s47)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n2,n3,n2,n3,n2,n3,n0,n2,x26,s47, 1.000)
       deallocate(s47)
c
       allocate(d1(n2+1:n3,n0+1:n1,n1+1:n3,n2+1:n3))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n1,n3,n2,n3,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder1423(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n1,n1,n3,n0,n2,t2b,d2)
       allocate(s48(n1+1:n3,n0+1:n2,n1+1:n3,n2+1:n3))
       i1=k4*k3
       i2=k2*k3
       i3=k1*k4
       call egemm(i1,i2,i3,d1,d2,s48)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n1,n3,n2,n3,n1,n3,n0,n2,x2,s48,-1.000)
       deallocate(s48)
c
       allocate(b1(n2+1:n3,n0+1:n2))
       call reorder12(0,n3,0,n3,
     & n2,n3,n0,n2,fockb,b1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s49(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,b1,d2,s49)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x5,s49, 1.000)
       deallocate(s49)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n2,n2,n3,fockb,b1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s50(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,b1,d2,s50)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n2,n3,n2,n3,n1,n3,n0,n1,x6,s50,-1.000)
       deallocate(s50)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder142356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
       allocate(u18(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k2*k4*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u18)
       deallocate(d1)
       deallocate(f2)
c
       call sum234516(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x30,u18,1.000)
       deallocate(u18)
c
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(s51(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k2*k3
       i3=k4*k1
       call egemm(i1,i2,i3,d1,d2,s51)
       deallocate(d1)
       deallocate(d2)
c
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x5,s51,-1.000)
       deallocate(s51)
c
       allocate(d1(n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n2,n3,n0,n1,intm,d1)
       allocate(d2(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder4312(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n0,n2,n2,n3,n1,n3,t2b,d2)
       allocate(s52(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4
       i2=k3*k4
       i3=k2*k1
       call egemm(i1,i2,i3,d1,d2,s52)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n2,n3,n2,n3,n1,n3,n0,n1,x6,s52, 1.000)
       deallocate(s52)
c
       allocate(d1(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u19(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k4
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u19)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x36(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       x36=0.0d0
       call sum356241(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x36,u19,1.000)
       deallocate(u19)
c
       allocate(d1(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s53(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k1*k2
       i3=k3*k4
       call egemm(i1,i2,i3,d1,d2,s53)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n1,n3,n0,n2,n0,n1,x5,s53, 1.000)
       deallocate(s53)
c
       allocate(d1(n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n2,n3,n1,n3,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder2314(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n2,n2,n3,n0,n1,t2b,d2)
       allocate(s54(n2+1:n3,n0+1:n1,n2+1:n3,n1+1:n3))
       i1=k3*k4
       i2=k1*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,d2,s54)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n2,n3,n2,n3,n1,n3,n0,n1,x6,s54,-1.000)
       deallocate(s54)
c
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u20(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u20)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x37(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x37=0.0d0
       call sum346251(n0,n2,n0,n2,n1,n3,n0,n2,n0,n2,n0,n1,x37,u20,1.000)
       deallocate(u20)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder142356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
       allocate(u21(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k2*k4*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u21)
       deallocate(d1)
       deallocate(f2)
c
       call sum234516(n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,x33,u21,1.000)
       deallocate(u21)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s55(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s55)
       deallocate(d1)
       deallocate(d2)
c
       call sum2431(n0,n2,n1,n3,n0,n2,n0,n1,x5,s55,-1.000)
       deallocate(s55)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u22(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u22)
       deallocate(d1)
       deallocate(d2)
c
       call sum456231(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x36,u22,1.000)
       deallocate(u22)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder123456(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,t3d,f2)
       allocate(u23(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k2*k2*k2*k4
       i3=k4*k4
       call egemm(i1,i2,i3,d1,f2,u23)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x38(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       x38=0.0d0
       call sum345621(n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,x38,u23,1.000)
       deallocate(u23)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s56(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s56)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n2,n3,n2,n3,n1,n3,n0,n1,x6,s56,-1.000)
       deallocate(s56)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u24(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u24)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder546123(n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n0,n1,n2,n3,n0,n2,n0,n1,u24,f1)
!       allocate(h2(n0+1:n1,n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2))
!       call reorder83712456(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,t4c,h2)
!       allocate(z114(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k4
!       i2=k2*k2*k3*k4*k4
!       i3=k1*k3*k1
!       call egemm(i1,i2,i3,f1,h2,z114)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do m=n0+1,n1;do n=n0+1,n1
             sum=sum                                       !top two switched
     &     + (u24(c,j,i,f,m,n)*t4c(d,b,f,a,l,k,n,m)        !cjifmndbfalknm  (+0.500)
     &     - u24(d,j,i,f,m,n)*t4c(c,b,f,a,l,k,n,m)         !djifmncbfalknm  (-0.500)
     &     - u24(b,j,i,f,m,n)*t4c(d,c,f,a,l,k,n,m)         !bjifmndcfalknm  (-0.500)
     &     + u24(d,k,i,f,m,n)*t4c(c,b,f,a,l,j,n,m)         !dkifmncbfaljnm  (+0.500)
     &     - u24(c,k,i,f,m,n)*t4c(d,b,f,a,l,j,n,m)         !ckifmndbfaljnm  (-0.500)
     &     + u24(b,k,i,f,m,n)*t4c(d,c,f,a,l,j,n,m)         !bkifmndcfaljnm  (+0.500)
     &     - u24(d,l,i,f,m,n)*t4c(c,b,f,a,k,j,n,m)         !dlifmncbfakjnm  (-0.500)
     &     + u24(c,l,i,f,m,n)*t4c(d,b,f,a,k,j,n,m)         !clifmndbfakjnm  (+0.500)
     &     - u24(b,l,i,f,m,n)*t4c(d,c,f,a,k,j,n,m))/2.0d0  !blifmndcfakjnm  (-0.500)
             enddo;enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23456178(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z114,-0.500)
!       call sum13456278(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z114, 0.500)
!       call sum12456378(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z114,-0.500)
!       call sum23457168(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z114, 0.500)
!       call sum13457268(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z114,-0.500)
!       call sum12457368(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z114, 0.500)
!       call sum23467158(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z114,-0.500)
!       call sum13467258(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z114, 0.500)
!       call sum12467358(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z114,-0.500)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z114(d,b,a,l,k,c,j,i)      ! 13456278 (+0.500) top two switched
!     & -z114(c,b,a,l,k,d,j,i)       ! 23456178 (-0.500)
!     & -z114(d,c,a,l,k,b,j,i)       ! 12456378 (-0.500)
!     & +z114(c,b,a,l,j,d,k,i)       ! 23457168 (+0.500)
!     & -z114(d,b,a,l,j,c,k,i)       ! 13457268 (-0.500)
!     & +z114(d,c,a,l,j,b,k,i)       ! 12457368 (+0.500)
!     & -z114(c,b,a,k,j,d,l,i)       ! 23467158 (-0.500)
!     & +z114(d,b,a,k,j,c,l,i)       ! 13467258 (+0.500)
!     & -z114(d,c,a,k,j,b,l,i))/2.0d0! 12467358 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z114)
c
       allocate(f1(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder465123(n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n2,n3,n0,n2,n0,n1,u24,f1)
       allocate(d2(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,t2b,d2)
       allocate(u98(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k4*k1
       i2=k2*k4
       i3=k1*k3
       call egemm(i1,i2,i3,f1,d2,u98)
       deallocate(f1)
       deallocate(d2)
c
      call sum341256(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x27,u98,-1.000)
       deallocate(u98)
c
       allocate(f1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder541236(n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,n0,n1,n0,n1,u24,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s110(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k4
       i3=k3*k1
       call egemm1(i1,i3,f1,b2,s110)
       deallocate(f1)
       deallocate(b2)
c
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s110,-1.000)
       deallocate(s110)
c
       allocate(f1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder541236(n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,n0,n1,n0,n1,u24,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u60(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k4*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u60)
       deallocate(f1)
       deallocate(b2)
       deallocate(u24)
c
      call sum423561(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,x31,u60,-1.000)
       deallocate(u60)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(h2(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,
     & n0+1:n2,n0+1:n1))
       call reorder34712568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,t4c,h2)
       allocate(u25(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k2*k2*k4*k4
       i3=k1*k3*k3
       call egemm(i1,i2,i3,d1,h2,u25)
       deallocate(d1)
       deallocate(h2)
c
       call sum234561(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x30,u25,0.500)
       deallocate(u25)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,t2b,d2)
       allocate(s57(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s57)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n2,n3,n0,n2,x7,s57, 1.000)
c
       allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4312(n2,n3,n0,n2,n1,n3,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,s57,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s111(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s111)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n1,n3,n2,n3,n1,n3,n0,n2,x2,s111,-1.000)
       deallocate(s111)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3412(n2,n3,n0,n2,n1,n3,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,s57,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s109(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s109)
       deallocate(d1)
       deallocate(b2)
       deallocate(s57)
c
       call sum4123(n0,n1,n2,n3,n0,n2,n0,n1,x1,s109, 1.000)
       deallocate(s109)
c
       allocate(d1(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u26(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u26)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder546123(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
!     & n0,n1,n2,n3,n0,n2,n2,n3,n0,n2,n0,n1,u26,f1)
!       allocate(h2(n0+1:n1,n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2))
!       call reorder81523467(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n1,n2,n3,n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,t4d,h2)
!       allocate(z117(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k4
!       i2=k2*k2*k3*k4*k4
!       i3=k2*k4*k1
!       call egemm(i1,i2,i3,f1,h2,z117)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     - u26(d,j,i,f,m,n)*t4d(f,c,b,a,n,l,k,m)  !djifmnfcbanlkm  (-1.000)
     &     + u26(c,j,i,f,m,n)*t4d(f,d,b,a,n,l,k,m)  !cjifmnfdbanlkm  (+1.000)
     &     - u26(b,j,i,f,m,n)*t4d(f,d,c,a,n,l,k,m)  !bjifmnfdcanlkm  (-1.000)
     &     + u26(d,k,i,f,m,n)*t4d(f,c,b,a,n,l,j,m)  !dkifmnfcbanljm  (+1.000)
     &     - u26(c,k,i,f,m,n)*t4d(f,d,b,a,n,l,j,m)  !ckifmnfdbanljm  (-1.000)
     &     + u26(b,k,i,f,m,n)*t4d(f,d,c,a,n,l,j,m)  !bkifmnfdcanljm  (+1.000)
     &     - u26(d,l,i,f,m,n)*t4d(f,c,b,a,n,k,j,m)  !dlifmnfcbankjm  (-1.000)
     &     + u26(c,l,i,f,m,n)*t4d(f,d,b,a,n,k,j,m)  !clifmnfdbankjm  (+1.000)
     &     - u26(b,l,i,f,m,n)*t4d(f,d,c,a,n,k,j,m)  !blifmnfdcankjm  (-1.000)
             enddo;enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23456178(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z117,-1.000)
!       call sum13456278(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z117, 1.000)
!       call sum12456378(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z117,-1.000)
!       call sum23457168(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z117, 1.000)
!       call sum13457268(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z117,-1.000)
!       call sum12457368(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z117, 1.000)
!       call sum23467158(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z117,-1.000)
!       call sum13467258(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z117, 1.000)
!       call sum12467358(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z117,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z117(c,b,a,l,k,d,j,i)       ! 23456178 (-1.000)
!     & +z117(d,b,a,l,k,c,j,i)       ! 13456278 (+1.000)
!     & -z117(d,c,a,l,k,b,j,i)       ! 12456378 (-1.000)
!     & +z117(c,b,a,l,j,d,k,i)       ! 23457168 (+1.000)
!     & -z117(d,b,a,l,j,c,k,i)       ! 13457268 (-1.000)
!     & +z117(d,c,a,l,j,b,k,i)       ! 12457368 (+1.000)
!     & -z117(c,b,a,k,j,d,l,i)       ! 23467158 (-1.000)
!     & +z117(d,b,a,k,j,c,l,i)       ! 13467258 (+1.000)
!     & -z117(d,c,a,k,j,b,l,i)       ! 12467358 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z117)
c
       allocate(f1(n0+1:n1,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder564123(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n0,n2,n2,n3,n2,n3,n0,n2,n0,n1,u26,f1)
       allocate(d2(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder4312(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n0,n2,n2,n3,n1,n3,t2b,d2)
       allocate(u103(n2+1:n3,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k4*k4
       i2=k3*k4
       i3=k2*k1
       call egemm(i1,i2,i3,f1,d2,u103)
       deallocate(f1)
       deallocate(d2)
c
      call sum341256(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x23,u103,1.000)
       deallocate(u103)
c
       allocate(f1(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n2,n3,n2,n3,n0,n2,n0,n1,n0,n2,u26,f1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u100(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k4
       i2=k2*k3
       i3=k4*k1
       call egemm(i1,i2,i3,f1,d2,u100)
       deallocate(f1)
       deallocate(d2)
c
       call sum342561(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,
     & u100,-1.000)
       deallocate(u100)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder465123(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n1,u26,f1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(u99(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k4*k1
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,f1,d2,u99)
       deallocate(f1)
       deallocate(d2)
c
      call sum341256(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x27,u99,-1.000)
       deallocate(u99)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder465123(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n1,u26,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s139(n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k4*k1
       i3=k2*k4
       call egemm1(i1,i3,f1,b2,s139)
       deallocate(f1)
       deallocate(b2)
c
       x1=x1+s139
       deallocate(s139)
c
       allocate(f1(n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder654123(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n2,n0,n1,n2,n3,n2,n3,n0,n2,n0,n1,u26,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u83(n2+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k4*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u83)
       deallocate(f1)
       deallocate(b2)
c
      call sum312456(n0,n1,n2,n3,n2,n3,n2,n3,n0,n2,n0,n1,x34,u83,-1.000)
       deallocate(u83)
c
!       allocate(f2(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
!       call reorder612345(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,t3c,f2)
!       allocate(z91(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k4*k4
!       i2=k2*k2*k3*k4
!       i3=k4*k1
!       call egemm(i1,i2,i3,x34,f2,z91)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n1
             sum=sum
     &     + x34(m,f,d,c,j,i)*t3c(f,b,a,l,k,m)      !mfdcjifbalkm    (+1.000)
     &     - x34(m,f,d,b,j,i)*t3c(f,c,a,l,k,m)      !mfdbjifcalkm    (-1.000)
     &     - x34(m,f,c,d,j,i)*t3c(f,b,a,l,k,m)      !mfcdjifbalkm    (-1.000)
     &     + x34(m,f,b,d,j,i)*t3c(f,c,a,l,k,m)      !mfbdjifcalkm    (+1.000)
     &     + x34(m,f,c,b,j,i)*t3c(f,d,a,l,k,m)      !mfcbjifdalkm    (+1.000)
     &     - x34(m,f,b,c,j,i)*t3c(f,d,a,l,k,m)      !mfbcjifdalkm    (-1.000)
     &     - x34(m,f,d,c,k,i)*t3c(f,b,a,l,j,m)      !mfdckifbaljm    (-1.000)
     &     + x34(m,f,d,b,k,i)*t3c(f,c,a,l,j,m)      !mfdbkifcaljm    (+1.000)
     &     + x34(m,f,c,d,k,i)*t3c(f,b,a,l,j,m)      !mfcdkifbaljm    (+1.000)
     &     - x34(m,f,b,d,k,i)*t3c(f,c,a,l,j,m)      !mfbdkifcaljm    (-1.000)
     &     - x34(m,f,c,b,k,i)*t3c(f,d,a,l,j,m)      !mfcbkifdaljm    (-1.000)
     &     + x34(m,f,b,c,k,i)*t3c(f,d,a,l,j,m)      !mfbckifdaljm    (+1.000)
     &     + x34(m,f,d,c,l,i)*t3c(f,b,a,k,j,m)      !mfdclifbakjm    (+1.000)
     &     - x34(m,f,d,b,l,i)*t3c(f,c,a,k,j,m)      !mfdblifcakjm    (-1.000)
     &     - x34(m,f,c,d,l,i)*t3c(f,b,a,k,j,m)      !mfcdlifbakjm    (-1.000)
     &     + x34(m,f,b,d,l,i)*t3c(f,c,a,k,j,m)      !mfbdlifcakjm    (+1.000)
     &     + x34(m,f,c,b,l,i)*t3c(f,d,a,k,j,m)      !mfcblifdakjm    (+1.000)
     &     - x34(m,f,b,c,l,i)*t3c(f,d,a,k,j,m)      !mfbclifdakjm    (-1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34561278(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91, 1.000)
!       call sum24561378(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91,-1.000)
!       call sum34562178(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91,-1.000)
!       call sum24563178(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91, 1.000)
!       call sum14562378(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91, 1.000)
!       call sum14563278(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91,-1.000)
!       call sum34571268(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91,-1.000)
!       call sum24571368(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91, 1.000)
!       call sum34572168(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91, 1.000)
!       call sum24573168(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91,-1.000)
!       call sum14572368(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91,-1.000)
!       call sum14573268(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91, 1.000)
!       call sum34671258(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91, 1.000)
!       call sum24671358(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91,-1.000)
!       call sum34672158(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91,-1.000)
!       call sum24673158(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91, 1.000)
!       call sum14672358(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91, 1.000)
!       call sum14673258(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z91,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z91(b,a,l,k,d,c,j,i)       ! 34561278 (+1.000)
!     & -z91(c,a,l,k,d,b,j,i)       ! 24561378 (-1.000)
!     & -z91(b,a,l,k,c,d,j,i)       ! 34562178 (-1.000)
!     & +z91(c,a,l,k,b,d,j,i)       ! 24563178 (+1.000)
!     & +z91(d,a,l,k,c,b,j,i)       ! 14562378 (+1.000)
!     & -z91(d,a,l,k,b,c,j,i)       ! 14563278 (-1.000)
!     & -z91(b,a,l,j,d,c,k,i)       ! 34571268 (-1.000)
!     & +z91(c,a,l,j,d,b,k,i)       ! 24571368 (+1.000)
!     & +z91(b,a,l,j,c,d,k,i)       ! 34572168 (+1.000)
!     & -z91(c,a,l,j,b,d,k,i)       ! 24573168 (-1.000)
!     & -z91(d,a,l,j,c,b,k,i)       ! 14572368 (-1.000)
!     & +z91(d,a,l,j,b,c,k,i)       ! 14573268 (+1.000)
!     & +z91(b,a,k,j,d,c,l,i)       ! 34671258 (+1.000)
!     & -z91(c,a,k,j,d,b,l,i)       ! 24671358 (-1.000)
!     & -z91(b,a,k,j,c,d,l,i)       ! 34672158 (-1.000)
!     & +z91(c,a,k,j,b,d,l,i)       ! 24673158 (+1.000)
!     & +z91(d,a,k,j,c,b,l,i)       ! 14672358 (+1.000)
!     & -z91(d,a,k,j,b,c,l,i)       ! 14673258 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z91)
       deallocate(x34)
c
       allocate(f1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder451236(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n1,n2,n3,n0,n2,n0,n1,n0,n2,u26,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u81(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k4*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u81)
       deallocate(f1)
       deallocate(b2)
c
       call sum523461(n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,x32,u81,1.000)
c
!       allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder461235(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,t3c,f2)
!       allocate(z88(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k4
!       i2=k2*k3*k4*k4
!       i3=k1*k2
!       call egemm(i1,i2,i3,x32,f2,z88)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     + x32(n,m,d,k,j,i)*t3c(c,b,a,n,l,m)      !nmdkjicbanlm    (+1.000)
     &     - x32(n,m,c,k,j,i)*t3c(d,b,a,n,l,m)      !nmckjidbanlm    (-1.000)
     &     + x32(n,m,b,k,j,i)*t3c(d,c,a,n,l,m)      !nmbkjidcanlm    (+1.000)
     &     - x32(n,m,d,l,j,i)*t3c(c,b,a,n,k,m)      !nmdljicbankm    (-1.000)
     &     + x32(n,m,c,l,j,i)*t3c(d,b,a,n,k,m)      !nmcljidbankm    (+1.000)
     &     - x32(n,m,b,l,j,i)*t3c(d,c,a,n,k,m)      !nmbljidcankm    (-1.000)
     &     - x32(n,m,d,j,k,i)*t3c(c,b,a,n,l,m)      !nmdjkicbanlm    (-1.000)
     &     + x32(n,m,c,j,k,i)*t3c(d,b,a,n,l,m)      !nmcjkidbanlm    (+1.000)
     &     - x32(n,m,b,j,k,i)*t3c(d,c,a,n,l,m)      !nmbjkidcanlm    (-1.000)
     &     + x32(n,m,d,j,l,i)*t3c(c,b,a,n,k,m)      !nmdjlicbankm    (+1.000)
     &     - x32(n,m,c,j,l,i)*t3c(d,b,a,n,k,m)      !nmcjlidbankm    (-1.000)
     &     + x32(n,m,b,j,l,i)*t3c(d,c,a,n,k,m)      !nmbjlidcankm    (+1.000)
     &     + x32(n,m,d,l,k,i)*t3c(c,b,a,n,j,m)      !nmdlkicbanjm    (+1.000)
     &     - x32(n,m,c,l,k,i)*t3c(d,b,a,n,j,m)      !nmclkidbanjm    (-1.000)
     &     + x32(n,m,b,l,k,i)*t3c(d,c,a,n,j,m)      !nmblkidcanjm    (+1.000)
     &     - x32(n,m,d,k,l,i)*t3c(c,b,a,n,j,m)      !nmdklicbanjm    (-1.000)
     &     + x32(n,m,c,k,l,i)*t3c(d,b,a,n,j,m)      !nmcklidbanjm    (+1.000)
     &     - x32(n,m,b,k,l,i)*t3c(d,c,a,n,j,m)      !nmbklidcanjm    (-1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23451678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88, 1.000)
!       call sum13452678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88,-1.000)
!       call sum12453678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88, 1.000)
!       call sum23461578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88,-1.000)
!       call sum13462578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88, 1.000)
!       call sum12463578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88,-1.000)
!       call sum23451768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88,-1.000)
!       call sum13452768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88, 1.000)
!       call sum12453768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88,-1.000)
!       call sum23461758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88, 1.000)
!       call sum13462758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88,-1.000)
!       call sum12463758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88, 1.000)
!       call sum23471568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88, 1.000)
!       call sum13472568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88,-1.000)
!       call sum12473568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88, 1.000)
!       call sum23471658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88,-1.000)
!       call sum13472658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88, 1.000)
!       call sum12473658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z88,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z88(c,b,a,l,d,k,j,i)       ! 23451678 (+1.000)
!     & -z88(d,b,a,l,c,k,j,i)       ! 13452678 (-1.000)
!     & +z88(d,c,a,l,b,k,j,i)       ! 12453678 (+1.000)
!     & -z88(c,b,a,k,d,l,j,i)       ! 23461578 (-1.000)
!     & +z88(d,b,a,k,c,l,j,i)       ! 13462578 (+1.000)
!     & -z88(d,c,a,k,b,l,j,i)       ! 12463578 (-1.000)
!     & -z88(c,b,a,l,d,j,k,i)       ! 23451768 (-1.000)
!     & +z88(d,b,a,l,c,j,k,i)       ! 13452768 (+1.000)
!     & -z88(d,c,a,l,b,j,k,i)       ! 12453768 (-1.000)
!     & +z88(c,b,a,k,d,j,l,i)       ! 23461758 (+1.000)
!     & -z88(d,b,a,k,c,j,l,i)       ! 13462758 (-1.000)
!     & +z88(d,c,a,k,b,j,l,i)       ! 12463758 (+1.000)
!     & +z88(c,b,a,j,d,l,k,i)       ! 23471568 (+1.000)
!     & -z88(d,b,a,j,c,l,k,i)       ! 13472568 (-1.000)
!     & +z88(d,c,a,j,b,l,k,i)       ! 12473568 (+1.000)
!     & -z88(c,b,a,j,d,k,l,i)       ! 23471658 (-1.000)
!     & +z88(d,b,a,j,c,k,l,i)       ! 13472658 (+1.000)
!     & -z88(d,c,a,j,b,k,l,i)       ! 12473658 (-1.000)
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
       deallocate(x32)
c
       allocate(f1(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder623415(n0,n2,n0,n1,n2,n3,n0,n2,n0,n1,n0,n2,
     & n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,u81,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u117(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u117)
       deallocate(f1)
       deallocate(b2)
       deallocate(u81)
c
      call sum213456(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x27,u117,1.000)
       deallocate(u117)
c
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z247(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k4*k4
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x27,d2,z247)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - x27(m,d,c,k,j,i)*t2b(b,a,l,m)          !mdckjibalm      (-1.000)
     &     + x27(m,d,b,k,j,i)*t2b(c,a,l,m)          !mdbkjicalm      (+1.000)
     &     + x27(m,c,d,k,j,i)*t2b(b,a,l,m)          !mcdkjibalm      (+1.000)
     &     - x27(m,b,d,k,j,i)*t2b(c,a,l,m)          !mbdkjicalm      (-1.000)
     &     - x27(m,c,b,k,j,i)*t2b(d,a,l,m)          !mcbkjidalm      (-1.000)
     &     + x27(m,b,c,k,j,i)*t2b(d,a,l,m)          !mbckjidalm      (+1.000)
     &     + x27(m,d,c,l,j,i)*t2b(b,a,k,m)          !mdcljibakm      (+1.000)
     &     - x27(m,d,b,l,j,i)*t2b(c,a,k,m)          !mdbljicakm      (-1.000)
     &     - x27(m,c,d,l,j,i)*t2b(b,a,k,m)          !mcdljibakm      (-1.000)
     &     + x27(m,b,d,l,j,i)*t2b(c,a,k,m)          !mbdljicakm      (+1.000)
     &     + x27(m,c,b,l,j,i)*t2b(d,a,k,m)          !mcbljidakm      (+1.000)
     &     - x27(m,b,c,l,j,i)*t2b(d,a,k,m)          !mbcljidakm      (-1.000)
     &     + x27(m,d,c,j,k,i)*t2b(b,a,l,m)          !mdcjkibalm      (+1.000)
     &     - x27(m,d,b,j,k,i)*t2b(c,a,l,m)          !mdbjkicalm      (-1.000)
     &     - x27(m,c,d,j,k,i)*t2b(b,a,l,m)          !mcdjkibalm      (-1.000)
     &     + x27(m,b,d,j,k,i)*t2b(c,a,l,m)          !mbdjkicalm      (+1.000)
     &     + x27(m,c,b,j,k,i)*t2b(d,a,l,m)          !mcbjkidalm      (+1.000)
     &     - x27(m,b,c,j,k,i)*t2b(d,a,l,m)          !mbcjkidalm      (-1.000)
     &     - x27(m,d,c,j,l,i)*t2b(b,a,k,m)          !mdcjlibakm      (-1.000)
     &     + x27(m,d,b,j,l,i)*t2b(c,a,k,m)          !mdbjlicakm      (+1.000)
     &     + x27(m,c,d,j,l,i)*t2b(b,a,k,m)          !mcdjlibakm      (+1.000)
     &     - x27(m,b,d,j,l,i)*t2b(c,a,k,m)          !mbdjlicakm      (-1.000)
     &     - x27(m,c,b,j,l,i)*t2b(d,a,k,m)          !mcbjlidakm      (-1.000)
     &     + x27(m,b,c,j,l,i)*t2b(d,a,k,m)          !mbcjlidakm      (+1.000)
     &     - x27(m,d,c,l,k,i)*t2b(b,a,j,m)          !mdclkibajm      (-1.000)
     &     + x27(m,d,b,l,k,i)*t2b(c,a,j,m)          !mdblkicajm      (+1.000)
     &     + x27(m,c,d,l,k,i)*t2b(b,a,j,m)          !mcdlkibajm      (+1.000)
     &     - x27(m,b,d,l,k,i)*t2b(c,a,j,m)          !mbdlkicajm      (-1.000)
     &     - x27(m,c,b,l,k,i)*t2b(d,a,j,m)          !mcblkidajm      (-1.000)
     &     + x27(m,b,c,l,k,i)*t2b(d,a,j,m)          !mbclkidajm      (+1.000)
     &     + x27(m,d,c,k,l,i)*t2b(b,a,j,m)          !mdcklibajm      (+1.000)
     &     - x27(m,d,b,k,l,i)*t2b(c,a,j,m)          !mdbklicajm      (-1.000)
     &     - x27(m,c,d,k,l,i)*t2b(b,a,j,m)          !mcdklibajm      (-1.000)
     &     + x27(m,b,d,k,l,i)*t2b(c,a,j,m)          !mbdklicajm      (+1.000)
     &     + x27(m,c,b,k,l,i)*t2b(d,a,j,m)          !mcbklidajm      (+1.000)
     &     - x27(m,b,c,k,l,i)*t2b(d,a,j,m)          !mbcklidajm      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34512678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum24513678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum34521678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum24531678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum14523678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum14532678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum34612578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum24613578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum34621578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum24631578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum14623578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum14632578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum34512768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum24513768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum34521768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum24531768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum14523768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum14532768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum34612758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum24613758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum34621758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum24631758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum14623758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum14632758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum34712568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum24713568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum34721568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum24731568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum14723568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum14732568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum34712658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum24713658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum34721658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
!       call sum24731658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum14723658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247, 1.000)
!       call sum14732658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z247,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z247(b,a,l,d,c,k,j,i)       ! 34512678 (-1.000)
!     & +z247(c,a,l,d,b,k,j,i)       ! 24513678 (+1.000)
!     & +z247(b,a,l,c,d,k,j,i)       ! 34521678 (+1.000)
!     & -z247(c,a,l,b,d,k,j,i)       ! 24531678 (-1.000)
!     & -z247(d,a,l,c,b,k,j,i)       ! 14523678 (-1.000)
!     & +z247(d,a,l,b,c,k,j,i)       ! 14532678 (+1.000)
!     & +z247(b,a,k,d,c,l,j,i)       ! 34612578 (+1.000)
!     & -z247(c,a,k,d,b,l,j,i)       ! 24613578 (-1.000)
!     & -z247(b,a,k,c,d,l,j,i)       ! 34621578 (-1.000)
!     & +z247(c,a,k,b,d,l,j,i)       ! 24631578 (+1.000)
!     & +z247(d,a,k,c,b,l,j,i)       ! 14623578 (+1.000)
!     & -z247(d,a,k,b,c,l,j,i)       ! 14632578 (-1.000)
!     & +z247(b,a,l,d,c,j,k,i)       ! 34512768 (+1.000)
!     & -z247(c,a,l,d,b,j,k,i)       ! 24513768 (-1.000)
!     & -z247(b,a,l,c,d,j,k,i)       ! 34521768 (-1.000)
!     & +z247(c,a,l,b,d,j,k,i)       ! 24531768 (+1.000)
!     & +z247(d,a,l,c,b,j,k,i)       ! 14523768 (+1.000)
!     & -z247(d,a,l,b,c,j,k,i)       ! 14532768 (-1.000)
!     & -z247(b,a,k,d,c,j,l,i)       ! 34612758 (-1.000)
!     & +z247(c,a,k,d,b,j,l,i)       ! 24613758 (+1.000)
!     & +z247(b,a,k,c,d,j,l,i)       ! 34621758 (+1.000)
!     & -z247(c,a,k,b,d,j,l,i)       ! 24631758 (-1.000)
!     & -z247(d,a,k,c,b,j,l,i)       ! 14623758 (-1.000)
!     & +z247(d,a,k,b,c,j,l,i)       ! 14632758 (+1.000)
!     & -z247(b,a,j,d,c,l,k,i)       ! 34712568 (-1.000)
!     & +z247(c,a,j,d,b,l,k,i)       ! 24713568 (+1.000)
!     & +z247(b,a,j,c,d,l,k,i)       ! 34721568 (+1.000)
!     & -z247(c,a,j,b,d,l,k,i)       ! 24731568 (-1.000)
!     & -z247(d,a,j,c,b,l,k,i)       ! 14723568 (-1.000)
!     & +z247(d,a,j,b,c,l,k,i)       ! 14732568 (+1.000)
!     & +z247(b,a,j,d,c,k,l,i)       ! 34712658 (+1.000)
!     & -z247(c,a,j,d,b,k,l,i)       ! 24713658 (-1.000)
!     & -z247(b,a,j,c,d,k,l,i)       ! 34721658 (-1.000)
!     & +z247(c,a,j,b,d,k,l,i)       ! 24731658 (+1.000)
!     & +z247(d,a,j,c,b,k,l,i)       ! 14723658 (+1.000)
!     & -z247(d,a,j,b,c,k,l,i)       ! 14732658 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z247)
       deallocate(x27)
c
       allocate(f1(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n2,n3,n2,n3,n0,n2,n0,n1,n0,n2,u26,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u62(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k4*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u62)
       deallocate(f1)
       deallocate(b2)
       deallocate(u26)
c
      call sum423561(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x36,u62,-1.000)
c
       allocate(f1(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder623145(n1,n3,n2,n3,n2,n3,n0,n2,n0,n1,n0,n2,
     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,u62,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u116(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u116)
       deallocate(f1)
       deallocate(b2)
c
       call sum213456(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x23,
     & u116,-1.000)
       deallocate(u116)
c
!       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
!       allocate(z208(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k4*k4
!       i2=k2*k2*k4
!       i3=k4
!       call egemm(i1,i2,i3,x23,d2,z208)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum
     &     + x23(f,d,c,a,j,i)*t2c(f,b,l,k)          !fdcajifblk      (+1.000)
     &     - x23(f,d,b,a,j,i)*t2c(f,c,l,k)          !fdbajifclk      (-1.000)
     &     - x23(f,c,d,a,j,i)*t2c(f,b,l,k)          !fcdajifblk      (-1.000)
     &     + x23(f,b,d,a,j,i)*t2c(f,c,l,k)          !fbdajifclk      (+1.000)
     &     + x23(f,c,b,a,j,i)*t2c(f,d,l,k)          !fcbajifdlk      (+1.000)
     &     - x23(f,b,c,a,j,i)*t2c(f,d,l,k)          !fbcajifdlk      (-1.000)
     &     - x23(f,d,c,a,k,i)*t2c(f,b,l,j)          !fdcakifblj      (-1.000)
     &     + x23(f,d,b,a,k,i)*t2c(f,c,l,j)          !fdbakifclj      (+1.000)
     &     + x23(f,c,d,a,k,i)*t2c(f,b,l,j)          !fcdakifblj      (+1.000)
     &     - x23(f,b,d,a,k,i)*t2c(f,c,l,j)          !fbdakifclj      (-1.000)
     &     - x23(f,c,b,a,k,i)*t2c(f,d,l,j)          !fcbakifdlj      (-1.000)
     &     + x23(f,b,c,a,k,i)*t2c(f,d,l,j)          !fbcakifdlj      (+1.000)
     &     + x23(f,d,c,a,l,i)*t2c(f,b,k,j)          !fdcalifbkj      (+1.000)
     &     - x23(f,d,b,a,l,i)*t2c(f,c,k,j)          !fdbalifckj      (-1.000)
     &     - x23(f,c,d,a,l,i)*t2c(f,b,k,j)          !fcdalifbkj      (-1.000)
     &     + x23(f,b,d,a,l,i)*t2c(f,c,k,j)          !fbdalifckj      (+1.000)
     &     + x23(f,c,b,a,l,i)*t2c(f,d,k,j)          !fcbalifdkj      (+1.000)
     &     - x23(f,b,c,a,l,i)*t2c(f,d,k,j)          !fbcalifdkj      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum35612478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208, 1.000)
!       call sum25613478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208,-1.000)
!       call sum35621478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208,-1.000)
!       call sum25631478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208, 1.000)
!       call sum15623478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208, 1.000)
!       call sum15632478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208,-1.000)
!       call sum35712468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208,-1.000)
!       call sum25713468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208, 1.000)
!       call sum35721468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208, 1.000)
!       call sum25731468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208,-1.000)
!       call sum15723468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208,-1.000)
!       call sum15732468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208, 1.000)
!       call sum36712458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208, 1.000)
!       call sum26713458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208,-1.000)
!       call sum36721458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208,-1.000)
!       call sum26731458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208, 1.000)
!       call sum16723458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208, 1.000)
!       call sum16732458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z208,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z208(b,l,k,d,c,a,j,i)       ! 35612478 (+1.000)
!     & -z208(c,l,k,d,b,a,j,i)       ! 25613478 (-1.000)
!     & -z208(b,l,k,c,d,a,j,i)       ! 35621478 (-1.000)
!     & +z208(c,l,k,b,d,a,j,i)       ! 25631478 (+1.000)
!     & +z208(d,l,k,c,b,a,j,i)       ! 15623478 (+1.000)
!     & -z208(d,l,k,b,c,a,j,i)       ! 15632478 (-1.000)
!     & -z208(b,l,j,d,c,a,k,i)       ! 35712468 (-1.000)
!     & +z208(c,l,j,d,b,a,k,i)       ! 25713468 (+1.000)
!     & +z208(b,l,j,c,d,a,k,i)       ! 35721468 (+1.000)
!     & -z208(c,l,j,b,d,a,k,i)       ! 25731468 (-1.000)
!     & -z208(d,l,j,c,b,a,k,i)       ! 15723468 (-1.000)
!     & +z208(d,l,j,b,c,a,k,i)       ! 15732468 (+1.000)
!     & +z208(b,k,j,d,c,a,l,i)       ! 36712458 (+1.000)
!     & -z208(c,k,j,d,b,a,l,i)       ! 26713458 (-1.000)
!     & -z208(b,k,j,c,d,a,l,i)       ! 36721458 (-1.000)
!     & +z208(c,k,j,b,d,a,l,i)       ! 26731458 (+1.000)
!     & +z208(d,k,j,c,b,a,l,i)       ! 16723458 (+1.000)
!     & -z208(d,k,j,b,c,a,l,i)       ! 16732458 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z208)
       deallocate(x23)
c
       allocate(f1(n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder263145(n1,n3,n2,n3,n2,n3,n0,n2,n0,n1,n0,n2,
     & n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,u62,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u115(n0+1:n2,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u115)
       deallocate(f1)
       deallocate(b2)
       deallocate(u62)
c
      call sum512346(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,u115,1.000)
       deallocate(u115)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u27(n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k3
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u27)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder546123(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
!     & n0,n1,n1,n3,n0,n2,n1,n3,n0,n2,n0,n1,u27,f1)
!       allocate(h2(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       call reorder84512367(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n1,n1,n3,n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t4d,h2)
!       allocate(z118(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3
!       i2=k2*k2*k4*k4*k4
!       i3=k2*k3*k1
!       call egemm(i1,i2,i3,f1,h2,z118)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     - u27(a,j,i,e,m,n)*t4d(d,c,b,e,n,l,k,m)  !ajiemndcbenlkm  (-1.000)
     &     + u27(a,k,i,e,m,n)*t4d(d,c,b,e,n,l,j,m)  !akiemndcbenljm  (+1.000)
     &     - u27(a,l,i,e,m,n)*t4d(d,c,b,e,n,k,j,m)  !aliemndcbenkjm  (-1.000)
             enddo;enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12356478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z118,-1.000)
!       call sum12357468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z118, 1.000)
!       call sum12367458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z118,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z118(d,c,b,l,k,a,j,i)       ! 12356478 (-1.000)
!     & +z118(d,c,b,l,j,a,k,i)       ! 12357468 (+1.000)
!     & -z118(d,c,b,k,j,a,l,i)       ! 12367458 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z118)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n1,n3,n0,n2,n0,n1,n0,n2,u27,f1)
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(u101(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,f1,d2,u101)
       deallocate(f1)
       deallocate(d2)
c
       call sum243561(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,
     & u101,-1.000)
       deallocate(u101)
c
c
       allocate(f1(n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder654123(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
     & n0,n2,n0,n1,n1,n3,n1,n3,n0,n2,n0,n1,u27,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u84(n2+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u84)
       deallocate(f1)
       deallocate(b2)
c
      call sum312456(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,x31,u84,-1.000)
       deallocate(u84)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n1,n3,n0,n2,n0,n1,n0,n2,u27,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s116(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3
       i3=k3*k1
       call egemm1(i1,i3,f1,b2,s116)
       deallocate(f1)
       deallocate(b2)
       deallocate(u27)
c
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x5,s116, 1.000)
       deallocate(s116)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s58(n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k2
       i3=k3*k4
       call egemm(i1,i2,i3,d1,d2,s58)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n0,n1,n0,n2,n0,n1,x13,s58, 1.000)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder3412(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s58,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u102(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u102)
       deallocate(d1)
       deallocate(d2)
c
      call sum234156(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,u102,1.000)
       deallocate(u102)
c
       allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       call reorder4312(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n2,n0,n1,n0,n2,n0,n1,s58,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s138(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s138)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s138,-1.000)
       deallocate(s138)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder3412(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s58,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s115(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s115)
       deallocate(d1)
       deallocate(b2)
       deallocate(s58)
c
       call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x5,s115,-1.000)
       deallocate(s115)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(h2(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,
     & n0+1:n2,n0+1:n2))
       call reorder14823567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
     & n0,n1,n2,n3,n1,n3,n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,t4d,h2)
       allocate(u28(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2*k2*k2*k4*k4
       i3=k1*k3*k4
       call egemm(i1,i2,i3,d1,h2,u28)
       deallocate(d1)
       deallocate(h2)
c
       allocate(x39(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       x39=0.0d0
      call sum234561(n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,x39,u28, 1.000)
       deallocate(u28)
c
       allocate(d1(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n2,n3,n0,n1,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder2314(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n2,n2,n3,n0,n1,t2b,d2)
       allocate(s59(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4
       i2=k1*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,d2,s59)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n2,n3,n2,n3,n0,n1,x14,s59,-1.000)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder3412(n2,n3,n0,n1,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s59,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u105(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u105)
       deallocate(d1)
       deallocate(d2)
c the correspongind term u104 in t4b has an explicite change and note in that part of the code.
       call sum345126(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x22,
     & u105,-1.000)
       deallocate(u105)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder3412(n2,n3,n0,n1,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s59,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s137(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s137)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n2,n3,n0,n2,n0,n1,x1,s137,-1.000)
       deallocate(s137)
c
       allocate(d1(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       call reorder4312(n2,n3,n0,n1,n2,n3,n0,n1,
     & n0,n1,n2,n3,n2,n3,n0,n1,s59,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s117(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s117)
       deallocate(d1)
       deallocate(b2)
       deallocate(s59)
c
       call sum3124(n2,n3,n2,n3,n1,n3,n0,n1,x6,s117, 1.000)
       deallocate(s117)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s60(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s60)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n1,n3,n0,n1,x12,s60, 1.000)
       deallocate(s60)
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
       allocate(h2(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,
     & n0+1:n2,n0+1:n1))
       call reorder14523678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
     & n0,n1,n2,n3,n1,n3,n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,t4d,h2)
       allocate(u29(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k2*k2*k4*k4
       i3=k2*k3*k4
       call egemm(i1,i2,i3,d1,h2,u29)
       deallocate(d1)
       deallocate(h2)
c
      call sum234561(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x30,u29, 1.000)
       deallocate(u29)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,t2b,d2)
       allocate(s61(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k2*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s61)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n2,n3,n0,n2,x18,s61, 1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3412(n2,n3,n0,n2,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,s61,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u104(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u104)
       deallocate(d1)
       deallocate(d2)
c
       call sum345126(n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,x28,
     & u104,-1.000)
       deallocate(u104)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder4312(n2,n3,n0,n2,n2,n3,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,s61,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s142(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s142)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n2,n3,n0,n2,x26,s142,-1.000)
       deallocate(s142)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3412(n2,n3,n0,n2,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,s61,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s140(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s140)
       deallocate(d1)
       deallocate(b2)
       deallocate(s61)
c
       call sum4123(n0,n2,n2,n3,n0,n2,n0,n2,x25,s140,-1.000)
       deallocate(s140)
c
       allocate(d1(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n1,n3,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder1423(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n1,n1,n3,n0,n2,t2b,d2)
       allocate(s62(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k2*k3
       i3=k1*k4
       call egemm(i1,i2,i3,d1,d2,s62)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n1,n3,n1,n3,n0,n2,x15,s62,-1.000)
c
       allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n1,n3,n0,n2,n1,n3,n0,n2,
     & n0,n2,n1,n3,n1,n3,n0,n2,s62,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s143(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s143)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s143, 1.000)
       deallocate(s143)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder3412(n1,n3,n0,n2,n1,n3,n0,n2,
     & n1,n3,n0,n2,n1,n3,n0,n2,s62,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s113(n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s113)
       deallocate(d1)
       deallocate(b2)
       deallocate(s62)
c
       call sum4123(n0,n2,n1,n3,n0,n2,n0,n1,x5,s113,-1.000)
       deallocate(s113)
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
       allocate(s63(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4
       i2=k3*k4
       i3=k2*k1
       call egemm(i1,i2,i3,d1,d2,s63)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n2,n3,n1,n3,n2,n3,n1,n3,x16,s63, 1.000)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       call reorder3412(n2,n3,n1,n3,n2,n3,n1,n3,
     & n2,n3,n1,n3,n2,n3,n1,n3,s63,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s141(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s141)
       deallocate(d1)
       deallocate(b2)
c
       call sum4123(n1,n3,n2,n3,n1,n3,n0,n2,x2,s141, 1.000)
       deallocate(s141)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       call reorder4312(n2,n3,n1,n3,n2,n3,n1,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,s63,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s114(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s114)
       deallocate(d1)
       deallocate(b2)
       deallocate(s63)
c
       call sum4123(n2,n3,n2,n3,n1,n3,n0,n1,x6,s114, 1.000)
       deallocate(s114)
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
       allocate(u30(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u30)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder546123(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n0,n2,n1,n3,n0,n2,n0,n1,u30,f1)
!       allocate(h2(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       call reorder61523478(n2,n3,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n0,n2,n2,n3,n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t4e,h2)
!       allocate(z131(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3
!       i2=k2*k2*k4*k4*k4
!       i3=k2*k4*k2
!       call egemm(i1,i2,i3,f1,h2,z131)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n2;do n=n0+1,n2
             sum=sum
     &     + (u30(a,k,i,f,m,n)*t4e(f,d,c,b,n,m,l,j)       !akifmnfdcbnmlj  (+0.500)
     &     - u30(a,j,i,f,m,n)*t4e(f,d,c,b,n,m,l,k)        !ajifmnfdcbnmlk  (-0.500)
     &     - u30(a,l,i,f,m,n)*t4e(f,d,c,b,n,m,k,j))/2.0d0 !alifmnfdcbnmkj  (-0.500)
             enddo;enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12356478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z131,-0.500)
!       call sum12357468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z131, 0.500)
!       call sum12367458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z131,-0.500)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z131(d,c,b,l,j,a,k,i)      ! 12357468 (+0.500) top two switched
!     & -z131(d,c,b,l,k,a,j,i)       ! 12356478 (-0.500)
!     & -z131(d,c,b,k,j,a,l,i))/2.0d0! 12367458 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z131)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder465123(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n0,n2,n1,n3,n0,n2,n0,n1,u30,f1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(u108(n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k2
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,f1,d2,u108)
       deallocate(f1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder314256(n2,n3,n0,n2,n0,n2,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,u108,f1)
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z304(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,f1,d2,z304)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     - u108(c,l,m,a,j,i)*t2c(d,b,m,k)         !clmajidbmk      (-1.000)
     &     - u108(d,k,m,a,j,i)*t2c(c,b,m,l)         !dkmajicbml      (-1.000)
     &     + u108(d,l,m,a,j,i)*t2c(c,b,m,k)         !dlmajicbmk      (+1.000)
     &     + u108(c,k,m,a,j,i)*t2c(d,b,m,l)         !ckmajidbml      (+1.000)
     &     + u108(c,l,m,a,k,i)*t2c(d,b,m,j)         !clmakidbmj      (+1.000)
     &     + u108(d,j,m,a,k,i)*t2c(c,b,m,l)         !djmakicbml      (+1.000)
     &     - u108(d,l,m,a,k,i)*t2c(c,b,m,j)         !dlmakicbmj      (-1.000)
     &     - u108(c,j,m,a,k,i)*t2c(d,b,m,l)         !cjmakidbml      (-1.000)
     &     - u108(c,k,m,a,l,i)*t2c(d,b,m,j)         !ckmalidbmj      (-1.000)
     &     - u108(d,j,m,a,l,i)*t2c(c,b,m,k)         !djmalicbmk      (-1.000)
     &     + u108(d,k,m,a,l,i)*t2c(c,b,m,j)         !dkmalicbmj      (+1.000)
     &     + u108(c,j,m,a,l,i)*t2c(d,b,m,k)         !cjmalidbmk      (+1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13624578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z304,-1.000)
!       call sum23514678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z304,-1.000)
!       call sum23614578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z304, 1.000)
!       call sum13524678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z304, 1.000)
!       call sum13724568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z304, 1.000)
!       call sum23514768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z304, 1.000)
!       call sum23714568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z304,-1.000)
!       call sum13524768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z304,-1.000)
!       call sum13724658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z304,-1.000)
!       call sum23614758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z304,-1.000)
!       call sum23714658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z304, 1.000)
!       call sum13624758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z304, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z304(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!     & -z304(c,b,l,d,a,k,j,i)       ! 23514678 (-1.000)
!     & +z304(c,b,k,d,a,l,j,i)       ! 23614578 (+1.000)
!     & +z304(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & +z304(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!     & +z304(c,b,l,d,a,j,k,i)       ! 23514768 (+1.000)
!     & -z304(c,b,j,d,a,l,k,i)       ! 23714568 (-1.000)
!     & -z304(d,b,l,c,a,j,k,i)       ! 13524768 (-1.000)
!     & -z304(d,b,j,c,a,k,l,i)       ! 13724658 (-1.000)
!     & -z304(c,b,k,d,a,j,l,i)       ! 23614758 (-1.000)
!     & +z304(c,b,j,d,a,k,l,i)       ! 23714658 (+1.000)
!     & +z304(d,b,k,c,a,j,l,i)       ! 13624758 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z304)
       deallocate(u108)
c
       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,n0,n2,u30,f1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(u107(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,f1,d2,u107)
       deallocate(f1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder613245(n2,n3,n0,n2,n1,n3,n0,n2,n0,n1,n0,n2,
!     & n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,u107,f1)
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z303(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,f1,d2,z303)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - u107(b,l,a,j,i,n)*t2c(d,c,n,k)         !blajindcnk      (-1.000)
     &     + u107(b,k,a,j,i,n)*t2c(d,c,n,l)         !bkajindcnl      (+1.000)
     &     + u107(b,l,a,k,i,n)*t2c(d,c,n,j)         !blakindcnj      (+1.000)
     &     - u107(b,j,a,k,i,n)*t2c(d,c,n,l)         !bjakindcnl      (-1.000)
     &     - u107(b,k,a,l,i,n)*t2c(d,c,n,j)         !bkalindcnj      (-1.000)
     &     + u107(b,j,a,l,i,n)*t2c(d,c,n,k)         !bjalindcnk      (+1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12634578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z303,-1.000)
!       call sum12534678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z303, 1.000)
!       call sum12734568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z303, 1.000)
!       call sum12534768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z303,-1.000)
!       call sum12734658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z303,-1.000)
!       call sum12634758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z303, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z303(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & +z303(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & +z303(d,c,j,b,a,l,k,i)       ! 12734568 (+1.000)
!     & -z303(d,c,l,b,a,j,k,i)       ! 12534768 (-1.000)
!     & -z303(d,c,j,b,a,k,l,i)       ! 12734658 (-1.000)
!     & +z303(d,c,k,b,a,j,l,i)       ! 12634758 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z303)
       deallocate(u107)
c
       allocate(f1(n0+1:n2,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder564123(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,u30,f1)
       allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder4312(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
       allocate(u106(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k4
       i2=k4*k4
       i3=k2*k2
       call egemm(i1,i2,i3,f1,d2,u106)
       deallocate(f1)
       deallocate(d2)
c
!       allocate(f1(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder312456(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,
!     & n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,u106,f1)
!       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
!       allocate(z302(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k4*k4
!       i2=k2*k2*k4
!       i3=k4
!       call egemm(i1,i2,i3,f1,d2,z302)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum
     &     + (u106(d,b,f,a,j,i)*t2c(f,c,l,k)        !dbfajifclk      (+0.500)
     &     - u106(c,b,f,a,j,i)*t2c(f,d,l,k)         !cbfajifdlk      (-0.500)
     &     - u106(d,c,f,a,j,i)*t2c(f,b,l,k)         !dcfajifblk      (-0.500)
     &     + u106(c,b,f,a,k,i)*t2c(f,d,l,j)         !cbfakifdlj      (+0.500)
     &     - u106(d,b,f,a,k,i)*t2c(f,c,l,j)         !dbfakifclj      (-0.500)
     &     + u106(d,c,f,a,k,i)*t2c(f,b,l,j)         !dcfakifblj      (+0.500)
     &     - u106(c,b,f,a,l,i)*t2c(f,d,k,j)         !cbfalifdkj      (-0.500)
     &     + u106(d,b,f,a,l,i)*t2c(f,c,k,j)         !dbfalifckj      (+0.500)
     &     - u106(d,c,f,a,l,i)*t2c(f,b,k,j))/2.0d0  !dcfalifbkj      (-0.500)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum15623478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z302,-0.500)
!       call sum25613478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z302, 0.500)
!       call sum35612478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z302,-0.500)
!       call sum15723468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z302, 0.500)
!       call sum25713468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z302,-0.500)
!       call sum35712468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z302, 0.500)
!       call sum16723458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z302,-0.500)
!       call sum26713458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z302, 0.500)
!       call sum36712458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z302,-0.500)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z302(c,l,k,d,b,a,j,i)      ! 25613478 (+0.500) top two switched
!     & -z302(d,l,k,c,b,a,j,i)       ! 15623478 (-0.500)
!     & -z302(b,l,k,d,c,a,j,i)       ! 35612478 (-0.500)
!     & +z302(d,l,j,c,b,a,k,i)       ! 15723468 (+0.500)
!     & -z302(c,l,j,d,b,a,k,i)       ! 25713468 (-0.500)
!     & +z302(b,l,j,d,c,a,k,i)       ! 35712468 (+0.500)
!     & -z302(d,k,j,c,b,a,l,i)       ! 16723458 (-0.500)
!     & +z302(c,k,j,d,b,a,l,i)       ! 26713458 (+0.500)
!     & -z302(b,k,j,d,c,a,l,i))/2.0d0! 36712458 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z302)
       deallocate(u106)
c
       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,n0,n2,u30,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s146(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3
       i3=k4*k2
       call egemm1(i1,i3,f1,b2,s146)
       deallocate(f1)
       deallocate(b2)
c
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x5,s146,-1.000)
       deallocate(s146)
c
       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,n0,n2,u30,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u89(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u89)
       deallocate(f1)
       deallocate(b2)
c
      call sum324561(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x36,u89,-1.000)
c
       allocate(f1(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder621345(n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,n0,n2,
     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,u89,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u121(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u121)
       deallocate(f1)
       deallocate(b2)
       deallocate(u89)
c
      call sum213456(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x29,u121,1.000)
       deallocate(u121)
c
!       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
!       allocate(z81(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k4*k4
!       i2=k2*k2*k4
!       i3=k4
!       call egemm(i1,i2,i3,x29,d2,z81)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum
     &     - x29(f,d,c,a,j,i)*t2c(f,b,l,k)          !fdcajifblk      (-1.000)
     &     + x29(f,d,b,a,j,i)*t2c(f,c,l,k)          !fdbajifclk      (+1.000)
     &     - x29(f,c,b,a,j,i)*t2c(f,d,l,k)          !fcbajifdlk      (-1.000)
     &     + x29(f,d,c,a,k,i)*t2c(f,b,l,j)          !fdcakifblj      (+1.000)
     &     - x29(f,d,b,a,k,i)*t2c(f,c,l,j)          !fdbakifclj      (-1.000)
     &     + x29(f,c,b,a,k,i)*t2c(f,d,l,j)          !fcbakifdlj      (+1.000)
     &     - x29(f,d,c,a,l,i)*t2c(f,b,k,j)          !fdcalifbkj      (-1.000)
     &     + x29(f,d,b,a,l,i)*t2c(f,c,k,j)          !fdbalifckj      (+1.000)
     &     - x29(f,c,b,a,l,i)*t2c(f,d,k,j)          !fcbalifdkj      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum35612478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z81,-1.000)
!       call sum25613478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z81, 1.000)
!       call sum15623478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z81,-1.000)
!       call sum35712468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z81, 1.000)
!       call sum25713468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z81,-1.000)
!       call sum15723468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z81, 1.000)
!       call sum36712458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z81,-1.000)
!       call sum26713458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z81, 1.000)
!       call sum16723458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z81,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z81(b,l,k,d,c,a,j,i)       ! 35612478 (-1.000)
!     & +z81(c,l,k,d,b,a,j,i)       ! 25613478 (+1.000)
!     & -z81(d,l,k,c,b,a,j,i)       ! 15623478 (-1.000)
!     & +z81(b,l,j,d,c,a,k,i)       ! 35712468 (+1.000)
!     & -z81(c,l,j,d,b,a,k,i)       ! 25713468 (-1.000)
!     & +z81(d,l,j,c,b,a,k,i)       ! 15723468 (+1.000)
!     & -z81(b,k,j,d,c,a,l,i)       ! 36712458 (-1.000)
!     & +z81(c,k,j,d,b,a,l,i)       ! 26713458 (+1.000)
!     & -z81(d,k,j,c,b,a,l,i)       ! 16723458 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z81)
       deallocate(x29)
c
       allocate(f1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder451236(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n2,n0,n1,n0,n2,u30,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u87(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u87)
       deallocate(f1)
       deallocate(b2)
       deallocate(u30)
c
      call sum523461(n0,n2,n0,n2,n1,n3,n0,n2,n0,n2,n0,n1,x37,u87,-1.000)
c
!       allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder451236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,t3d,f2)
!       allocate(z108(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3
!       i2=k2*k4*k4*k4
!       i3=k2*k2
!       call egemm(i1,i2,i3,x37,f2,z108)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2;do n=n0+1,n2
             sum=sum
     &     + (x37(n,m,a,k,j,i)*t3d(d,c,b,n,m,l)       !nmakjidcbnml    (+0.500)
     &     - x37(n,m,a,l,j,i)*t3d(d,c,b,n,m,k)        !nmaljidcbnmk    (-0.500)
     &     - x37(n,m,a,j,k,i)*t3d(d,c,b,n,m,l)        !nmajkidcbnml    (-0.500)
     &     + x37(n,m,a,j,l,i)*t3d(d,c,b,n,m,k)        !nmajlidcbnmk    (+0.500)
     &     + x37(n,m,a,l,k,i)*t3d(d,c,b,n,m,j)        !nmalkidcbnmj    (+0.500)
     &     - x37(n,m,a,k,l,i)*t3d(d,c,b,n,m,j))/2.0d0 !nmaklidcbnmj    (-0.500)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12354678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z108, 0.500)
!       call sum12364578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z108,-0.500)
!       call sum12354768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z108,-0.500)
!       call sum12364758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z108, 0.500)
!       call sum12374568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z108, 0.500)
!       call sum12374658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z108,-0.500)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z108(d,c,b,l,a,k,j,i)      ! 12354678 (+0.500)
!     & -z108(d,c,b,k,a,l,j,i)       ! 12364578 (-0.500)
!     & -z108(d,c,b,l,a,j,k,i)       ! 12354768 (-0.500)
!     & +z108(d,c,b,k,a,j,l,i)       ! 12364758 (+0.500)
!     & +z108(d,c,b,j,a,l,k,i)       ! 12374568 (+0.500)
!     & -z108(d,c,b,j,a,k,l,i))/2.0d0! 12374658 (-0.500)
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
       deallocate(x37)
c
       allocate(f1(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder263415(n0,n2,n0,n2,n1,n3,n0,n2,n0,n1,n0,n2,
     & n0,n2,n0,n2,n1,n3,n0,n2,n0,n2,n0,n1,u87,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u119(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2*k3*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u119)
       deallocate(f1)
       deallocate(b2)
       deallocate(u87)
c
      call sum213456(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,u119,1.000)
       deallocate(u119)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(h2(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,
     & n0+1:n2,n0+1:n2))
       call reorder12534678(n2,n3,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,t4e,h2)
       allocate(u31(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2*k2*k2*k4*k4
       i3=k2*k4*k4
       call egemm(i1,i2,i3,d1,h2,u31)
       deallocate(d1)
       deallocate(h2)
c
      call sum234561(n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,x39,u31,0.500)
       deallocate(u31)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z120(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k2*k4*k4
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x39,d2,z120)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - x39(n,c,b,l,k,j)*t2b(d,a,n,i)          !ncblkjdani      (-1.000)
     &     + x39(n,d,b,l,k,j)*t2b(c,a,n,i)          !ndblkjcani      (+1.000)
     &     - x39(n,d,c,l,k,j)*t2b(b,a,n,i)          !ndclkjbani      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14823567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z120,-1.000)
!       call sum24813567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z120, 1.000)
!       call sum34812567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z120,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z120(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & +z120(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z120(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z120)
       deallocate(x39)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s64(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s64)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n1,n3,n0,n1,x20,s64, 1.000)
c
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s64,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u111(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u111)
       deallocate(d1)
       deallocate(d2)
c
       call sum245136(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x24,
     & u111,-1.000)
       deallocate(u111)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n1,n3,n0,n1,n2,n3,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n1,s64,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s147(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s147)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s147,-1.000)
       deallocate(s147)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s64,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s145(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s145)
       deallocate(d1)
       deallocate(b2)
       deallocate(s64)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s145, 1.000)
       deallocate(s145)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u32(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u32)
       deallocate(d1)
       deallocate(d2)
c
      call sum245136(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x24,u32,-1.000)
       deallocate(u32)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder521346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(u33(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k2*k3*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,f2,u33)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x40(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x40=0.0d0
      call sum234651(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x40,u33, 1.000)
       deallocate(u33)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u34(n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k3
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u34)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x41(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       x41=0.0d0
       call sum456231(n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,x41,u34,1.000)
       deallocate(u34)
c
       allocate(b1(n2+1:n3,n0+1:n2))
       call reorder12(0,n3,0,n3,
     & n2,n3,n0,n2,fockb,b1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(s65(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,b1,d2,s65)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x3,s65, 1.000)
       deallocate(s65)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n2,n2,n3,fockb,b1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s66(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
       i1=k4
       i2=k2*k4*k4
       i3=k2
       call egemm(i1,i2,i3,b1,d2,s66)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n2,n3,n2,n3,n2,n3,n0,n2,x4,s66,-1.000)
       deallocate(s66)
c
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n0,n1,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u35(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u35)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x42(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x42=0.0d0
       call sum345261(n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,x42,u35,1.000)
       deallocate(u35)
c
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(f2(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder612345(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,t3c,f2)
       allocate(u36(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k2*k2*k3*k4
       i3=k4*k1
       call egemm(i1,i2,i3,d1,f2,u36)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x43(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x43=0.0d0
       call sum234561(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x43,u36,1.000)
       deallocate(u36)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(s67(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s67)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n0,n1,n2,n3,n0,n2,n0,n1,x1,s67, 1.000)
       deallocate(s67)
c
       allocate(d1(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u37(n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u37)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x44(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
       x44=0.0d0
       call sum356241(n0,n2,n1,n3,n2,n3,n1,n3,n0,n2,n0,n2,x44,u37,1.000)
       deallocate(u37)
c
       allocate(d1(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n1,n3,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder132456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n1,n3,n2,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
       allocate(u38(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k1*k2*k2*k4
       i3=k3*k4
       call egemm(i1,i2,i3,d1,f2,u38)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x45(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x45=0.0d0
       call sum245631(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x45,u38,1.000)
       deallocate(u38)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n1,n3,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(s68(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s68)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n1,n3,n2,n3,n1,n3,n0,n2,x2,s68, 1.000)
       deallocate(s68)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u39(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u39)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x46(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       x46=0.0d0
       call sum345261(n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,n0,n2,x46,u39,1.000)
       deallocate(u39)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder142356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
       allocate(u40(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k2*k3*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u40)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x47(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x47=0.0d0
       call sum234615(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x47,u40,1.000)
       deallocate(u40)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s69(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s69)
       deallocate(d1)
       deallocate(d2)
c
       call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x25,s69, 1.000)
       deallocate(s69)
c
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder4312(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
       allocate(s70(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k4*k4
       i3=k2*k2
       call egemm(i1,i2,i3,d1,d2,s70)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n2,n3,n2,n3,n2,n3,n0,n2,x4,s70, 0.500)
       deallocate(s70)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u41(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u41)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x48(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       x48=0.0d0
       call sum456231(n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,x48,u41,1.000)
       deallocate(u41)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(s71(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k2*k2
       i3=k4*k4
       call egemm(i1,i2,i3,d1,d2,s71)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n0,n2,n0,n2,x3,s71, 0.500)
       deallocate(s71)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder123456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
       allocate(u42(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k2*k2*k3
       i3=k4*k4
       call egemm(i1,i2,i3,d1,f2,u42)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder651234(n1,n3,n0,n2,n0,n2,n0,n1,n2,n3,n0,n2,
!     & n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,u42,f1)
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z151(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,f1,d2,z151)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + (u42(a,l,k,i,d,m)*t2c(c,b,m,j)         !alkidmcbmj      (+0.500)
     &     - u42(a,l,k,i,c,m)*t2c(d,b,m,j)          !alkicmdbmj      (-0.500)
     &     + u42(a,l,k,i,b,m)*t2c(d,c,m,j)          !alkibmdcmj      (+0.500)
     &     - u42(a,l,j,i,d,m)*t2c(c,b,m,k)          !aljidmcbmk      (-0.500)
     &     + u42(a,l,j,i,c,m)*t2c(d,b,m,k)          !aljicmdbmk      (+0.500)
     &     - u42(a,l,j,i,b,m)*t2c(d,c,m,k)          !aljibmdcmk      (-0.500)
     &     + u42(a,k,j,i,d,m)*t2c(c,b,m,l)          !akjidmcbml      (+0.500)
     &     - u42(a,k,j,i,c,m)*t2c(d,b,m,l)          !akjicmdbml      (-0.500)
     &     + u42(a,k,j,i,b,m)*t2c(d,c,m,l))/2.0d0   !akjibmdcml      (+0.500)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23714568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z151, 0.500)
!       call sum13724568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z151,-0.500)
!       call sum12734568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z151, 0.500)
!       call sum23614578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z151,-0.500)
!       call sum13624578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z151, 0.500)
!       call sum12634578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z151,-0.500)
!       call sum23514678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z151, 0.500)
!       call sum13524678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z151,-0.500)
!       call sum12534678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z151, 0.500)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z151(c,b,j,d,a,l,k,i)      ! 23714568 (+0.500)
!     & -z151(d,b,j,c,a,l,k,i)       ! 13724568 (-0.500)
!     & +z151(d,c,j,b,a,l,k,i)       ! 12734568 (+0.500)
!     & -z151(c,b,k,d,a,l,j,i)       ! 23614578 (-0.500)
!     & +z151(d,b,k,c,a,l,j,i)       ! 13624578 (+0.500)
!     & -z151(d,c,k,b,a,l,j,i)       ! 12634578 (-0.500)
!     & +z151(c,b,l,d,a,k,j,i)       ! 23514678 (+0.500)
!     & -z151(d,b,l,c,a,k,j,i)       ! 13524678 (-0.500)
!     & +z151(d,c,l,b,a,k,j,i))/2.0d0! 12534678 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z151)
       deallocate(u42)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(s72(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s72)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n2,n3,n2,n3,n2,n3,n0,n2,x26,s72,-1.000)
       deallocate(s72)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u43(n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k3
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u43)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder546123(n2,n3,n0,n2,n0,n2,n1,n3,n0,n1,n0,n2,
!     & n0,n1,n1,n3,n0,n2,n2,n3,n0,n2,n0,n2,u43,f1)
!       allocate(h2(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       call reorder73512468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t4c,h2)
!       allocate(z153(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k1*k2*k3*k4*k4
!       i3=k2*k3*k1
!       call egemm(i1,i2,i3,f1,h2,z153)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     + u43(d,k,j,e,m,n)*t4c(c,b,e,a,n,l,m,i)  !dkjemncbeanlmi  (+1.000)
     &     - u43(c,k,j,e,m,n)*t4c(d,b,e,a,n,l,m,i)  !ckjemndbeanlmi  (-1.000)
     &     + u43(b,k,j,e,m,n)*t4c(d,c,e,a,n,l,m,i)  !bkjemndceanlmi  (+1.000)
     &     - u43(d,l,j,e,m,n)*t4c(c,b,e,a,n,k,m,i)  !dljemncbeankmi  (-1.000)
     &     + u43(c,l,j,e,m,n)*t4c(d,b,e,a,n,k,m,i)  !cljemndbeankmi  (+1.000)
     &     - u43(b,l,j,e,m,n)*t4c(d,c,e,a,n,k,m,i)  !bljemndceankmi  (-1.000)
     &     + u43(d,l,k,e,m,n)*t4c(c,b,e,a,n,j,m,i)  !dlkemncbeanjmi  (+1.000)
     &     - u43(c,l,k,e,m,n)*t4c(d,b,e,a,n,j,m,i)  !clkemndbeanjmi  (-1.000)
     &     + u43(b,l,k,e,m,n)*t4c(d,c,e,a,n,j,m,i)  !blkemndceanjmi  (+1.000)
             enddo;enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23458167(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z153, 1.000)
!       call sum13458267(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z153,-1.000)
!       call sum12458367(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z153, 1.000)
!       call sum23468157(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z153,-1.000)
!       call sum13468257(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z153, 1.000)
!       call sum12468357(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z153,-1.000)
!       call sum23478156(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z153, 1.000)
!       call sum13478256(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z153,-1.000)
!       call sum12478356(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z153, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z153(c,b,a,l,i,d,k,j)       ! 23458167 (+1.000)
!     & -z153(d,b,a,l,i,c,k,j)       ! 13458267 (-1.000)
!     & +z153(d,c,a,l,i,b,k,j)       ! 12458367 (+1.000)
!     & -z153(c,b,a,k,i,d,l,j)       ! 23468157 (-1.000)
!     & +z153(d,b,a,k,i,c,l,j)       ! 13468257 (+1.000)
!     & -z153(d,c,a,k,i,b,l,j)       ! 12468357 (-1.000)
!     & +z153(c,b,a,j,i,d,l,k)       ! 23478156 (+1.000)
!     & -z153(d,b,a,j,i,c,l,k)       ! 13478256 (-1.000)
!     & +z153(d,c,a,j,i,b,l,k)       ! 12478356 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z153)
c
       allocate(f1(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder654123(n2,n3,n0,n2,n0,n2,n1,n3,n0,n1,n0,n2,
     & n0,n2,n0,n1,n1,n3,n2,n3,n0,n2,n0,n2,u43,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u92(n2+1:n3,n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u92)
       deallocate(f1)
       deallocate(b2)
c
      call sum312456(n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,x41,u92,-1.000)
       deallocate(u92)
c
!       allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder521346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
!       allocate(z136(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4*k4
!       i2=k1*k2*k3*k4
!       i3=k3*k1
!       call egemm(i1,i2,i3,x41,f2,z136)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + x41(m,e,d,c,k,j)*t3b(b,e,a,l,m,i)      !medckjbealmi    (+1.000)
     &     - x41(m,e,d,b,k,j)*t3b(c,e,a,l,m,i)      !medbkjcealmi    (-1.000)
     &     - x41(m,e,c,d,k,j)*t3b(b,e,a,l,m,i)      !mecdkjbealmi    (-1.000)
     &     + x41(m,e,b,d,k,j)*t3b(c,e,a,l,m,i)      !mebdkjcealmi    (+1.000)
     &     + x41(m,e,c,b,k,j)*t3b(d,e,a,l,m,i)      !mecbkjdealmi    (+1.000)
     &     - x41(m,e,b,c,k,j)*t3b(d,e,a,l,m,i)      !mebckjdealmi    (-1.000)
     &     - x41(m,e,d,c,l,j)*t3b(b,e,a,k,m,i)      !medcljbeakmi    (-1.000)
     &     + x41(m,e,d,b,l,j)*t3b(c,e,a,k,m,i)      !medbljceakmi    (+1.000)
     &     + x41(m,e,c,d,l,j)*t3b(b,e,a,k,m,i)      !mecdljbeakmi    (+1.000)
     &     - x41(m,e,b,d,l,j)*t3b(c,e,a,k,m,i)      !mebdljceakmi    (-1.000)
     &     - x41(m,e,c,b,l,j)*t3b(d,e,a,k,m,i)      !mecbljdeakmi    (-1.000)
     &     + x41(m,e,b,c,l,j)*t3b(d,e,a,k,m,i)      !mebcljdeakmi    (+1.000)
     &     + x41(m,e,d,c,l,k)*t3b(b,e,a,j,m,i)      !medclkbeajmi    (+1.000)
     &     - x41(m,e,d,b,l,k)*t3b(c,e,a,j,m,i)      !medblkceajmi    (-1.000)
     &     - x41(m,e,c,d,l,k)*t3b(b,e,a,j,m,i)      !mecdlkbeajmi    (-1.000)
     &     + x41(m,e,b,d,l,k)*t3b(c,e,a,j,m,i)      !mebdlkceajmi    (+1.000)
     &     + x41(m,e,c,b,l,k)*t3b(d,e,a,j,m,i)      !mecblkdeajmi    (+1.000)
     &     - x41(m,e,b,c,l,k)*t3b(d,e,a,j,m,i)      !mebclkdeajmi    (-1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34581267(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136, 1.000)
!       call sum24581367(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136,-1.000)
!       call sum34582167(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136,-1.000)
!       call sum24583167(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136, 1.000)
!       call sum14582367(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136, 1.000)
!       call sum14583267(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136,-1.000)
!       call sum34681257(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136,-1.000)
!       call sum24681357(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136, 1.000)
!       call sum34682157(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136, 1.000)
!       call sum24683157(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136,-1.000)
!       call sum14682357(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136,-1.000)
!       call sum14683257(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136, 1.000)
!       call sum34781256(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136, 1.000)
!       call sum24781356(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136,-1.000)
!       call sum34782156(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136,-1.000)
!       call sum24783156(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136, 1.000)
!       call sum14782356(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136, 1.000)
!       call sum14783256(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z136,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z136(b,a,l,i,d,c,k,j)       ! 34581267 (+1.000)
!     & -z136(c,a,l,i,d,b,k,j)       ! 24581367 (-1.000)
!     & -z136(b,a,l,i,c,d,k,j)       ! 34582167 (-1.000)
!     & +z136(c,a,l,i,b,d,k,j)       ! 24583167 (+1.000)
!     & +z136(d,a,l,i,c,b,k,j)       ! 14582367 (+1.000)
!     & -z136(d,a,l,i,b,c,k,j)       ! 14583267 (-1.000)
!     & -z136(b,a,k,i,d,c,l,j)       ! 34681257 (-1.000)
!     & +z136(c,a,k,i,d,b,l,j)       ! 24681357 (+1.000)
!     & +z136(b,a,k,i,c,d,l,j)       ! 34682157 (+1.000)
!     & -z136(c,a,k,i,b,d,l,j)       ! 24683157 (-1.000)
!     & -z136(d,a,k,i,c,b,l,j)       ! 14682357 (-1.000)
!     & +z136(d,a,k,i,b,c,l,j)       ! 14683257 (+1.000)
!     & +z136(b,a,j,i,d,c,l,k)       ! 34781256 (+1.000)
!     & -z136(c,a,j,i,d,b,l,k)       ! 24781356 (-1.000)
!     & -z136(b,a,j,i,c,d,l,k)       ! 34782156 (-1.000)
!     & +z136(c,a,j,i,b,d,l,k)       ! 24783156 (+1.000)
!     & +z136(d,a,j,i,c,b,l,k)       ! 14782356 (+1.000)
!     & -z136(d,a,j,i,b,c,l,k)       ! 14783256 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z136)
       deallocate(x41)
c
       allocate(f1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder541236(n2,n3,n0,n2,n0,n2,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n2,n3,n0,n2,n0,n2,n0,n2,u43,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s120(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2*k4
       i3=k3*k1
       call egemm1(i1,i3,f1,b2,s120)
       deallocate(f1)
       deallocate(b2)
c
       call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x3,s120, 1.000)
       deallocate(s120)
c
       allocate(f1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder541236(n2,n3,n0,n2,n0,n2,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n2,n3,n0,n2,n0,n2,n0,n2,u43,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u67(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2*k4*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u67)
       deallocate(f1)
       deallocate(b2)
c
      call sum423561(n0,n2,n1,n3,n2,n3,n1,n3,n0,n2,n0,n2,x44,u67,-1.000)
       deallocate(u67)
c
!       allocate(f2(n0+1:n2,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder431256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n1,n3,n2,n3,n2,n3,n0,n2,n0,n1,t3c,f2)
!       allocate(z142(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k3*k4
!       i2=k1*k2*k4*k4
!       i3=k3*k2
!       call egemm(i1,i2,i3,x44,f2,z142)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n2
             sum=sum
     &     + x44(m,e,d,a,k,j)*t3c(c,b,e,m,l,i)      !medakjcbemli    (+1.000)
     &     - x44(m,e,c,a,k,j)*t3c(d,b,e,m,l,i)      !mecakjdbemli    (-1.000)
     &     + x44(m,e,b,a,k,j)*t3c(d,c,e,m,l,i)      !mebakjdcemli    (+1.000)
     &     - x44(m,e,d,a,l,j)*t3c(c,b,e,m,k,i)      !medaljcbemki    (-1.000)
     &     + x44(m,e,c,a,l,j)*t3c(d,b,e,m,k,i)      !mecaljdbemki    (+1.000)
     &     - x44(m,e,b,a,l,j)*t3c(d,c,e,m,k,i)      !mebaljdcemki    (-1.000)
     &     + x44(m,e,d,a,l,k)*t3c(c,b,e,m,j,i)      !medalkcbemji    (+1.000)
     &     - x44(m,e,c,a,l,k)*t3c(d,b,e,m,j,i)      !mecalkdbemji    (-1.000)
     &     + x44(m,e,b,a,l,k)*t3c(d,c,e,m,j,i)      !mebalkdcemji    (+1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23581467(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z142, 1.000)
!       call sum13582467(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z142,-1.000)
!       call sum12583467(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z142, 1.000)
!       call sum23681457(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z142,-1.000)
!       call sum13682457(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z142, 1.000)
!       call sum12683457(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z142,-1.000)
!       call sum23781456(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z142, 1.000)
!       call sum13782456(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z142,-1.000)
!       call sum12783456(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z142, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z142(c,b,l,i,d,a,k,j)       ! 23581467 (+1.000)
!     & -z142(d,b,l,i,c,a,k,j)       ! 13582467 (-1.000)
!     & +z142(d,c,l,i,b,a,k,j)       ! 12583467 (+1.000)
!     & -z142(c,b,k,i,d,a,l,j)       ! 23681457 (-1.000)
!     & +z142(d,b,k,i,c,a,l,j)       ! 13682457 (+1.000)
!     & -z142(d,c,k,i,b,a,l,j)       ! 12683457 (-1.000)
!     & +z142(c,b,j,i,d,a,l,k)       ! 23781456 (+1.000)
!     & -z142(d,b,j,i,c,a,l,k)       ! 13782456 (-1.000)
!     & +z142(d,c,j,i,b,a,l,k)       ! 12783456 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z142)
       deallocate(x44)
c
       allocate(f1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder451236(n2,n3,n0,n2,n0,n2,n1,n3,n0,n1,n0,n2,
     & n1,n3,n0,n1,n2,n3,n0,n2,n0,n2,n0,n2,u43,f1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(u65(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2*k4*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,f1,b2,u65)
       deallocate(f1)
       deallocate(b2)
       deallocate(u43)
c
       call sum623451(n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,x42,u65,1.000)
c
       allocate(f1(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder623451(n0,n1,n0,n1,n2,n3,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,u65,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u114(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u114)
       deallocate(f1)
       deallocate(b2)
c
       call sum213456(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x22,
     & u114,-1.000)
       deallocate(u114)
c
       allocate(f1(n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder263451(n0,n1,n0,n1,n2,n3,n0,n2,n0,n2,n0,n2,
     & n0,n1,n0,n2,n2,n3,n0,n2,n0,n2,n0,n1,u65,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u112(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2*k4*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u112)
       deallocate(f1)
       deallocate(b2)
       deallocate(u65)
c
      call sum312456(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x24,u112,1.000)
       deallocate(u112)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(h2(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
     & n0+1:n2,n0+1:n1))
       call reorder13724568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n1,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t4c,h2)
       allocate(u44(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k2*k2*k3*k4
       i3=k1*k3*k4
       call egemm(i1,i2,i3,d1,h2,u44)
       deallocate(d1)
       deallocate(h2)
c
      call sum234561(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x43,u44,-1.000)
       deallocate(u44)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(s73(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s73)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n2,n3,n0,n2,x7,s73, 1.000)
c
       allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4312(n2,n3,n0,n2,n1,n3,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,s73,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s122(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s122)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n1,n3,n2,n3,n1,n3,n0,n2,x2,s122,-1.000)
       deallocate(s122)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3412(n2,n3,n0,n2,n1,n3,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,s73,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s119(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s119)
       deallocate(d1)
       deallocate(b2)
       deallocate(s73)
c
       call sum4123(n0,n1,n2,n3,n0,n2,n0,n1,x1,s119, 1.000)
       deallocate(s119)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u45(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u45)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder546123(n2,n3,n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n0,n2,n2,n3,n0,n2,n0,n2,u45,f1)
!       allocate(h2(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       call reorder61523478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n2,n2,n3,n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t4d,h2)
!       allocate(z156(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k1*k2*k3*k4*k4
!       i3=k2*k4*k2
!       call egemm(i1,i2,i3,f1,h2,z156)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n2;do n=n0+1,n2
             sum=sum
     &     + (u45(d,k,j,f,m,n)*t4d(f,c,b,a,n,m,l,i)        !dkjfmnfcbanmli  (+0.500)
     &     - u45(c,k,j,f,m,n)*t4d(f,d,b,a,n,m,l,i)         !ckjfmnfdbanmli  (-0.500)
     &     + u45(b,k,j,f,m,n)*t4d(f,d,c,a,n,m,l,i)         !bkjfmnfdcanmli  (+0.500)
     &     - u45(d,l,j,f,m,n)*t4d(f,c,b,a,n,m,k,i)         !dljfmnfcbanmki  (-0.500)
     &     + u45(c,l,j,f,m,n)*t4d(f,d,b,a,n,m,k,i)         !cljfmnfdbanmki  (+0.500)
     &     - u45(b,l,j,f,m,n)*t4d(f,d,c,a,n,m,k,i)         !bljfmnfdcanmki  (-0.500)
     &     + u45(d,l,k,f,m,n)*t4d(f,c,b,a,n,m,j,i)         !dlkfmnfcbanmji  (+0.500)
     &     - u45(c,l,k,f,m,n)*t4d(f,d,b,a,n,m,j,i)         !clkfmnfdbanmji  (-0.500)
     &     + u45(b,l,k,f,m,n)*t4d(f,d,c,a,n,m,j,i))/2.0d0  !blkfmnfdcanmji  (+0.500)
             enddo;enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23458167(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z156, 0.500)
!       call sum13458267(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z156,-0.500)
!       call sum12458367(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z156, 0.500)
!       call sum23468157(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z156,-0.500)
!       call sum13468257(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z156, 0.500)
!       call sum12468357(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z156,-0.500)
!       call sum23478156(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z156, 0.500)
!       call sum13478256(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z156,-0.500)
!       call sum12478356(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z156, 0.500)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z156(c,b,a,l,i,d,k,j)      ! 23458167 (+0.500)
!     & -z156(d,b,a,l,i,c,k,j)       ! 13458267 (-0.500)
!     & +z156(d,c,a,l,i,b,k,j)       ! 12458367 (+0.500)
!     & -z156(c,b,a,k,i,d,l,j)       ! 23468157 (-0.500)
!     & +z156(d,b,a,k,i,c,l,j)       ! 13468257 (+0.500)
!     & -z156(d,c,a,k,i,b,l,j)       ! 12468357 (-0.500)
!     & +z156(c,b,a,j,i,d,l,k)       ! 23478156 (+0.500)
!     & -z156(d,b,a,j,i,c,l,k)       ! 13478256 (-0.500)
!     & +z156(d,c,a,j,i,b,l,k))/2.0d0! 12478356 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z156)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder465123(n2,n3,n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,u45,f1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(u109(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4*k2
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,f1,d2,u109)
       deallocate(f1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
!       call reorder341256(n2,n3,n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,u109,f1)
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z305(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k2*k4*k4
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,f1,d2,z305)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     - u109(b,l,m,c,k,j)*t2b(d,a,m,i)         !blmckjdami      (-1.000)
     &     + u109(c,l,m,b,k,j)*t2b(d,a,m,i)         !clmbkjdami      (+1.000)
     &     + u109(b,l,m,d,k,j)*t2b(c,a,m,i)         !blmdkjcami      (+1.000)
     &     - u109(c,l,m,d,k,j)*t2b(b,a,m,i)         !clmdkjbami      (-1.000)
     &     - u109(d,l,m,b,k,j)*t2b(c,a,m,i)         !dlmbkjcami      (-1.000)
     &     + u109(d,l,m,c,k,j)*t2b(b,a,m,i)         !dlmckjbami      (+1.000)
     &     + u109(b,k,m,c,l,j)*t2b(d,a,m,i)         !bkmcljdami      (+1.000)
     &     - u109(c,k,m,b,l,j)*t2b(d,a,m,i)         !ckmbljdami      (-1.000)
     &     - u109(b,k,m,d,l,j)*t2b(c,a,m,i)         !bkmdljcami      (-1.000)
     &     + u109(c,k,m,d,l,j)*t2b(b,a,m,i)         !ckmdljbami      (+1.000)
     &     + u109(d,k,m,b,l,j)*t2b(c,a,m,i)         !dkmbljcami      (+1.000)
     &     - u109(d,k,m,c,l,j)*t2b(b,a,m,i)         !dkmcljbami      (-1.000)
     &     + u109(c,j,m,b,l,k)*t2b(d,a,m,i)         !cjmblkdami      (+1.000)
     &     - u109(b,j,m,c,l,k)*t2b(d,a,m,i)         !bjmclkdami      (-1.000)
     &     - u109(d,j,m,b,l,k)*t2b(c,a,m,i)         !djmblkcami      (-1.000)
     &     + u109(d,j,m,c,l,k)*t2b(b,a,m,i)         !djmclkbami      (+1.000)
     &     + u109(b,j,m,d,l,k)*t2b(c,a,m,i)         !bjmdlkcami      (+1.000)
     &     - u109(c,j,m,d,l,k)*t2b(b,a,m,i)         !cjmdlkbami      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14823567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305,-1.000)
!       call sum14832567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305, 1.000)
!       call sum24813567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305, 1.000)
!       call sum34812567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305,-1.000)
!       call sum24831567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305,-1.000)
!       call sum34821567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305, 1.000)
!       call sum14823657(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305, 1.000)
!       call sum14832657(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305,-1.000)
!       call sum24813657(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305,-1.000)
!       call sum34812657(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305, 1.000)
!       call sum24831657(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305, 1.000)
!       call sum34821657(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305,-1.000)
!       call sum14832756(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305, 1.000)
!       call sum14823756(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305,-1.000)
!       call sum24831756(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305,-1.000)
!       call sum34821756(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305, 1.000)
!       call sum24813756(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305, 1.000)
!       call sum34812756(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z305,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z305(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & +z305(d,a,i,b,c,l,k,j)       ! 14832567 (+1.000)
!     & +z305(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z305(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & -z305(c,a,i,b,d,l,k,j)       ! 24831567 (-1.000)
!     & +z305(b,a,i,c,d,l,k,j)       ! 34821567 (+1.000)
!     & +z305(d,a,i,c,b,k,l,j)       ! 14823657 (+1.000)
!     & -z305(d,a,i,b,c,k,l,j)       ! 14832657 (-1.000)
!     & -z305(c,a,i,d,b,k,l,j)       ! 24813657 (-1.000)
!     & +z305(b,a,i,d,c,k,l,j)       ! 34812657 (+1.000)
!     & +z305(c,a,i,b,d,k,l,j)       ! 24831657 (+1.000)
!     & -z305(b,a,i,c,d,k,l,j)       ! 34821657 (-1.000)
!     & +z305(d,a,i,b,c,j,l,k)       ! 14832756 (+1.000)
!     & -z305(d,a,i,c,b,j,l,k)       ! 14823756 (-1.000)
!     & -z305(c,a,i,b,d,j,l,k)       ! 24831756 (-1.000)
!     & +z305(b,a,i,c,d,j,l,k)       ! 34821756 (+1.000)
!     & +z305(c,a,i,d,b,j,l,k)       ! 24813756 (+1.000)
!     & -z305(b,a,i,d,c,j,l,k)       ! 34812756 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z305)
       deallocate(u109)
c
c
       allocate(f1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder541236(n2,n3,n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,u45,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s152(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2*k4
       i3=k4*k2
       call egemm1(i1,i3,f1,b2,s152)
       deallocate(f1)
       deallocate(b2)
c
       call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x3,s152,-1.000)
       deallocate(s152)
c
       allocate(f1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder541236(n2,n3,n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,u45,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u95(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u95)
       deallocate(f1)
       deallocate(b2)
c
      call sum324561(n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,x48,u95,-1.000)
       deallocate(u95)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
!       allocate(z149(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4*k4
!       i2=k1*k2*k3*k4
!       i3=k4*k2
!       call egemm(i1,i2,i3,x48,f2,z149)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x48(m,f,d,c,k,j)*t3c(f,b,a,m,l,i)      !mfdckjfbamli    (+1.000)
     &     - x48(m,f,d,b,k,j)*t3c(f,c,a,m,l,i)      !mfdbkjfcamli    (-1.000)
     &     - x48(m,f,c,d,k,j)*t3c(f,b,a,m,l,i)      !mfcdkjfbamli    (-1.000)
     &     + x48(m,f,b,d,k,j)*t3c(f,c,a,m,l,i)      !mfbdkjfcamli    (+1.000)
     &     + x48(m,f,c,b,k,j)*t3c(f,d,a,m,l,i)      !mfcbkjfdamli    (+1.000)
     &     - x48(m,f,b,c,k,j)*t3c(f,d,a,m,l,i)      !mfbckjfdamli    (-1.000)
     &     - x48(m,f,d,c,l,j)*t3c(f,b,a,m,k,i)      !mfdcljfbamki    (-1.000)
     &     + x48(m,f,d,b,l,j)*t3c(f,c,a,m,k,i)      !mfdbljfcamki    (+1.000)
     &     + x48(m,f,c,d,l,j)*t3c(f,b,a,m,k,i)      !mfcdljfbamki    (+1.000)
     &     - x48(m,f,b,d,l,j)*t3c(f,c,a,m,k,i)      !mfbdljfcamki    (-1.000)
     &     - x48(m,f,c,b,l,j)*t3c(f,d,a,m,k,i)      !mfcbljfdamki    (-1.000)
     &     + x48(m,f,b,c,l,j)*t3c(f,d,a,m,k,i)      !mfbcljfdamki    (+1.000)
     &     + x48(m,f,d,c,l,k)*t3c(f,b,a,m,j,i)      !mfdclkfbamji    (+1.000)
     &     - x48(m,f,d,b,l,k)*t3c(f,c,a,m,j,i)      !mfdblkfcamji    (-1.000)
     &     - x48(m,f,c,d,l,k)*t3c(f,b,a,m,j,i)      !mfcdlkfbamji    (-1.000)
     &     + x48(m,f,b,d,l,k)*t3c(f,c,a,m,j,i)      !mfbdlkfcamji    (+1.000)
     &     + x48(m,f,c,b,l,k)*t3c(f,d,a,m,j,i)      !mfcblkfdamji    (+1.000)
     &     - x48(m,f,b,c,l,k)*t3c(f,d,a,m,j,i)      !mfbclkfdamji    (-1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34581267(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149, 1.000)
!       call sum24581367(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149,-1.000)
!       call sum34582167(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149,-1.000)
!       call sum24583167(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149, 1.000)
!       call sum14582367(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149, 1.000)
!       call sum14583267(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149,-1.000)
!       call sum34681257(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149,-1.000)
!       call sum24681357(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149, 1.000)
!       call sum34682157(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149, 1.000)
!       call sum24683157(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149,-1.000)
!       call sum14682357(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149,-1.000)
!       call sum14683257(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149, 1.000)
!       call sum34781256(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149, 1.000)
!       call sum24781356(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149,-1.000)
!       call sum34782156(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149,-1.000)
!       call sum24783156(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149, 1.000)
!       call sum14782356(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149, 1.000)
!       call sum14783256(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z149,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z149(b,a,l,i,d,c,k,j)       ! 34581267 (+1.000)
!     & -z149(c,a,l,i,d,b,k,j)       ! 24581367 (-1.000)
!     & -z149(b,a,l,i,c,d,k,j)       ! 34582167 (-1.000)
!     & +z149(c,a,l,i,b,d,k,j)       ! 24583167 (+1.000)
!     & +z149(d,a,l,i,c,b,k,j)       ! 14582367 (+1.000)
!     & -z149(d,a,l,i,b,c,k,j)       ! 14583267 (-1.000)
!     & -z149(b,a,k,i,d,c,l,j)       ! 34681257 (-1.000)
!     & +z149(c,a,k,i,d,b,l,j)       ! 24681357 (+1.000)
!     & +z149(b,a,k,i,c,d,l,j)       ! 34682157 (+1.000)
!     & -z149(c,a,k,i,b,d,l,j)       ! 24683157 (-1.000)
!     & -z149(d,a,k,i,c,b,l,j)       ! 14682357 (-1.000)
!     & +z149(d,a,k,i,b,c,l,j)       ! 14683257 (+1.000)
!     & +z149(b,a,j,i,d,c,l,k)       ! 34781256 (+1.000)
!     & -z149(c,a,j,i,d,b,l,k)       ! 24781356 (-1.000)
!     & -z149(b,a,j,i,c,d,l,k)       ! 34782156 (-1.000)
!     & +z149(c,a,j,i,b,d,l,k)       ! 24783156 (+1.000)
!     & +z149(d,a,j,i,c,b,l,k)       ! 14782356 (+1.000)
!     & -z149(d,a,j,i,b,c,l,k)       ! 14783256 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z149)
       deallocate(x48)
c
       allocate(f1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder451236(n2,n3,n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,n0,n2,n0,n2,u45,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u93(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u93)
       deallocate(f1)
       deallocate(b2)
       deallocate(u45)
c
      call sum623451(n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,n0,n2,x46,u93,-1.000)
c
!       allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder451236(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n0,n2,n2,n3,n2,n3,n1,n3,n0,n1,t3c,f2)
!       allocate(z145(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k2*k4
!       i2=k1*k3*k4*k4
!       i3=k2*k2
!       call egemm(i1,i2,i3,x46,f2,z145)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2;do n=n0+1,n2
             sum=sum
     &     + (x46(n,m,c,l,k,j)*t3c(d,b,a,n,m,i)        !nmclkjdbanmi    (+0.500)
     &     - x46(n,m,d,l,k,j)*t3c(c,b,a,n,m,i)         !nmdlkjcbanmi    (-0.500)
     &     - x46(n,m,b,l,k,j)*t3c(d,c,a,n,m,i)         !nmblkjdcanmi    (-0.500)
     &     + x46(n,m,d,l,j,k)*t3c(c,b,a,n,m,i)         !nmdljkcbanmi    (+0.500)
     &     - x46(n,m,c,l,j,k)*t3c(d,b,a,n,m,i)         !nmcljkdbanmi    (-0.500)
     &     + x46(n,m,b,l,j,k)*t3c(d,c,a,n,m,i)         !nmbljkdcanmi    (+0.500)
     &     - x46(n,m,d,k,j,l)*t3c(c,b,a,n,m,i)         !nmdkjlcbanmi    (-0.500)
     &     + x46(n,m,c,k,j,l)*t3c(d,b,a,n,m,i)         !nmckjldbanmi    (+0.500)
     &     - x46(n,m,b,k,j,l)*t3c(d,c,a,n,m,i))/2.0d0  !nmbkjldcanmi    (-0.500)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23481567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z145,-0.500)
!       call sum13482567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z145, 0.500)
!       call sum12483567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z145,-0.500)
!       call sum23481576(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z145, 0.500)
!       call sum13482576(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z145,-0.500)
!       call sum12483576(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z145, 0.500)
!       call sum23481675(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z145,-0.500)
!       call sum13482675(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z145, 0.500)
!       call sum12483675(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z145,-0.500)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z145(d,b,a,i,c,l,k,j)      ! 13482567 (+0.500) top two switched
!     & -z145(c,b,a,i,d,l,k,j)       ! 23481567 (-0.500)
!     & -z145(d,c,a,i,b,l,k,j)       ! 12483567 (-0.500)
!     & +z145(c,b,a,i,d,l,j,k)       ! 23481576 (+0.500)
!     & -z145(d,b,a,i,c,l,j,k)       ! 13482576 (-0.500)
!     & +z145(d,c,a,i,b,l,j,k)       ! 12483576 (+0.500)
!     & -z145(c,b,a,i,d,k,j,l)       ! 23481675 (-0.500)
!     & +z145(d,b,a,i,c,k,j,l)       ! 13482675 (+0.500)
!     & -z145(d,c,a,i,b,k,j,l))/2.0d0! 12483675 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z145)
       deallocate(x46)
c
       allocate(f1(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder263451(n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,n0,n2,u93,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u120(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2*k4*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u120)
       deallocate(f1)
       deallocate(b2)
       deallocate(u93)
c
       call sum213456(n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,x28,
     & u120,-1.000)
       deallocate(u120)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z255(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k2*k4*k4
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x28,d2,z255)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     + x28(n,d,b,l,k,j)*t2b(c,a,n,i)          !ndblkjcani      (+1.000)
     &     - x28(n,d,c,l,k,j)*t2b(b,a,n,i)          !ndclkjbani      (-1.000)
     &     - x28(n,c,b,l,k,j)*t2b(d,a,n,i)          !ncblkjdani      (-1.000)
     &     + x28(n,b,c,l,k,j)*t2b(d,a,n,i)          !nbclkjdani      (+1.000)
     &     + x28(n,c,d,l,k,j)*t2b(b,a,n,i)          !ncdlkjbani      (+1.000)
     &     - x28(n,b,d,l,k,j)*t2b(c,a,n,i)          !nbdlkjcani      (-1.000)
     &     - x28(n,d,b,l,j,k)*t2b(c,a,n,i)          !ndbljkcani      (-1.000)
     &     + x28(n,d,c,l,j,k)*t2b(b,a,n,i)          !ndcljkbani      (+1.000)
     &     + x28(n,c,b,l,j,k)*t2b(d,a,n,i)          !ncbljkdani      (+1.000)
     &     - x28(n,b,c,l,j,k)*t2b(d,a,n,i)          !nbcljkdani      (-1.000)
     &     - x28(n,c,d,l,j,k)*t2b(b,a,n,i)          !ncdljkbani      (-1.000)
     &     + x28(n,b,d,l,j,k)*t2b(c,a,n,i)          !nbdljkcani      (+1.000)
     &     + x28(n,d,b,k,j,l)*t2b(c,a,n,i)          !ndbkjlcani      (+1.000)
     &     - x28(n,d,c,k,j,l)*t2b(b,a,n,i)          !ndckjlbani      (-1.000)
     &     - x28(n,c,b,k,j,l)*t2b(d,a,n,i)          !ncbkjldani      (-1.000)
     &     + x28(n,b,c,k,j,l)*t2b(d,a,n,i)          !nbckjldani      (+1.000)
     &     + x28(n,c,d,k,j,l)*t2b(b,a,n,i)          !ncdkjlbani      (+1.000)
     &     - x28(n,b,d,k,j,l)*t2b(c,a,n,i)          !nbdkjlcani      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum24813567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255, 1.000)
!       call sum34812567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255,-1.000)
!       call sum14823567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255,-1.000)
!       call sum14832567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255, 1.000)
!       call sum34821567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255, 1.000)
!       call sum24831567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255,-1.000)
!       call sum24813576(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255,-1.000)
!       call sum34812576(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255, 1.000)
!       call sum14823576(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255, 1.000)
!       call sum14832576(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255,-1.000)
!       call sum34821576(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255,-1.000)
!       call sum24831576(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255, 1.000)
!       call sum24813675(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255, 1.000)
!       call sum34812675(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255,-1.000)
!       call sum14823675(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255,-1.000)
!       call sum14832675(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255, 1.000)
!       call sum34821675(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255, 1.000)
!       call sum24831675(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z255,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z255(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z255(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & -z255(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & +z255(d,a,i,b,c,l,k,j)       ! 14832567 (+1.000)
!     & +z255(b,a,i,c,d,l,k,j)       ! 34821567 (+1.000)
!     & -z255(c,a,i,b,d,l,k,j)       ! 24831567 (-1.000)
!     & -z255(c,a,i,d,b,l,j,k)       ! 24813576 (-1.000)
!     & +z255(b,a,i,d,c,l,j,k)       ! 34812576 (+1.000)
!     & +z255(d,a,i,c,b,l,j,k)       ! 14823576 (+1.000)
!     & -z255(d,a,i,b,c,l,j,k)       ! 14832576 (-1.000)
!     & -z255(b,a,i,c,d,l,j,k)       ! 34821576 (-1.000)
!     & +z255(c,a,i,b,d,l,j,k)       ! 24831576 (+1.000)
!     & +z255(c,a,i,d,b,k,j,l)       ! 24813675 (+1.000)
!     & -z255(b,a,i,d,c,k,j,l)       ! 34812675 (-1.000)
!     & -z255(d,a,i,c,b,k,j,l)       ! 14823675 (-1.000)
!     & +z255(d,a,i,b,c,k,j,l)       ! 14832675 (+1.000)
!     & +z255(b,a,i,c,d,k,j,l)       ! 34821675 (+1.000)
!     & -z255(c,a,i,b,d,k,j,l)       ! 24831675 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z255)
       deallocate(x28)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(s74(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k2
       i3=k4*k4
       call egemm(i1,i2,i3,d1,d2,s74)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x17(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       x17=0.0d0
       call sum3421(n0,n2,n0,n2,n0,n2,n0,n2,x17,s74, 0.500)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder3412(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s74,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(u110(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,d1,d2,u110)
       deallocate(d1)
       deallocate(d2)
c
      call sum236145(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x24,u110,0.500)
       deallocate(u110)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder3412(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s74,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s151(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s151)
       deallocate(d1)
       deallocate(b2)
       deallocate(s74)
c
       call sum2134(n0,n2,n2,n3,n0,n2,n0,n2,x3,s151,-0.500)
       deallocate(s151)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(h2(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
     & n0+1:n2,n0+1:n1))
       call reorder12534678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
     & n0,n1,n2,n3,n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t4d,h2)
       allocate(u46(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k2*k2*k3*k4
       i3=k2*k4*k4
       call egemm(i1,i2,i3,d1,h2,u46)
       deallocate(d1)
       deallocate(h2)
c
      call sum234561(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x45,u46, 0.500)
       deallocate(u46)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z143(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x45,d2,z143)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + x45(m,b,a,l,k,i)*t2c(d,c,m,j)          !mbalkidcmj      (+1.000)
     &     - x45(m,c,a,l,k,i)*t2c(d,b,m,j)          !mcalkidbmj      (-1.000)
     &     + x45(m,d,a,l,k,i)*t2c(c,b,m,j)          !mdalkicbmj      (+1.000)
     &     - x45(m,b,a,l,j,i)*t2c(d,c,m,k)          !mbaljidcmk      (-1.000)
     &     + x45(m,c,a,l,j,i)*t2c(d,b,m,k)          !mcaljidbmk      (+1.000)
     &     - x45(m,d,a,l,j,i)*t2c(c,b,m,k)          !mdaljicbmk      (-1.000)
     &     + x45(m,b,a,k,j,i)*t2c(d,c,m,l)          !mbakjidcml      (+1.000)
     &     - x45(m,c,a,k,j,i)*t2c(d,b,m,l)          !mcakjidbml      (-1.000)
     &     + x45(m,d,a,k,j,i)*t2c(c,b,m,l)          !mdakjicbml      (+1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12734568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z143, 1.000)
!       call sum13724568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z143,-1.000)
!       call sum23714568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z143, 1.000)
!       call sum12634578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z143,-1.000)
!       call sum13624578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z143, 1.000)
!       call sum23614578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z143,-1.000)
!       call sum12534678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z143, 1.000)
!       call sum13524678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z143,-1.000)
!       call sum23514678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z143, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z143(d,c,j,b,a,l,k,i)       ! 12734568 (+1.000)
!     & -z143(d,b,j,c,a,l,k,i)       ! 13724568 (-1.000)
!     & +z143(c,b,j,d,a,l,k,i)       ! 23714568 (+1.000)
!     & -z143(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & +z143(d,b,k,c,a,l,j,i)       ! 13624578 (+1.000)
!     & -z143(c,b,k,d,a,l,j,i)       ! 23614578 (-1.000)
!     & +z143(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & -z143(d,b,l,c,a,k,j,i)       ! 13524678 (-1.000)
!     & +z143(c,b,l,d,a,k,j,i)       ! 23514678 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z143)
       deallocate(x45)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(s75(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s75)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n2,n3,n0,n2,x18,s75, 1.000)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder4312(n2,n3,n0,n2,n2,n3,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,s75,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s153(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s153)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n2,n3,n0,n2,x26,s153,-1.000)
       deallocate(s153)
c
!       allocate(f2(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder123456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
!       allocate(z47(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k1*k2*k2*k3*k4
!       i3=k4
!       call egemm(i1,i2,i3,x26,f2,z47)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     - x26(e,d,c,j)*t3c(e,b,a,l,k,i)          !edcjebalki      (-1.000)
     &     + x26(e,d,b,j)*t3c(e,c,a,l,k,i)          !edbjecalki      (+1.000)
     &     + x26(e,c,d,j)*t3c(e,b,a,l,k,i)          !ecdjebalki      (+1.000)
     &     - x26(e,b,d,j)*t3c(e,c,a,l,k,i)          !ebdjecalki      (-1.000)
     &     - x26(e,c,b,j)*t3c(e,d,a,l,k,i)          !ecbjedalki      (-1.000)
     &     + x26(e,b,c,j)*t3c(e,d,a,l,k,i)          !ebcjedalki      (+1.000)
     &     + x26(e,d,c,k)*t3c(e,b,a,l,j,i)          !edckebalji      (+1.000)
     &     - x26(e,d,b,k)*t3c(e,c,a,l,j,i)          !edbkecalji      (-1.000)
     &     - x26(e,c,d,k)*t3c(e,b,a,l,j,i)          !ecdkebalji      (-1.000)
     &     + x26(e,b,d,k)*t3c(e,c,a,l,j,i)          !ebdkecalji      (+1.000)
     &     + x26(e,c,b,k)*t3c(e,d,a,l,j,i)          !ecbkedalji      (+1.000)
     &     - x26(e,b,c,k)*t3c(e,d,a,l,j,i)          !ebckedalji      (-1.000)
     &     - x26(e,d,c,l)*t3c(e,b,a,k,j,i)          !edclebakji      (-1.000)
     &     + x26(e,d,b,l)*t3c(e,c,a,k,j,i)          !edblecakji      (+1.000)
     &     + x26(e,c,d,l)*t3c(e,b,a,k,j,i)          !ecdlebakji      (+1.000)
     &     - x26(e,b,d,l)*t3c(e,c,a,k,j,i)          !ebdlecakji      (-1.000)
     &     - x26(e,c,b,l)*t3c(e,d,a,k,j,i)          !ecbledakji      (-1.000)
     &     + x26(e,b,c,l)*t3c(e,d,a,k,j,i)          !ebcledakji      (+1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34568127(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47,-1.000)
!       call sum24568137(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47, 1.000)
!       call sum34568217(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47, 1.000)
!       call sum24568317(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47,-1.000)
!       call sum14568237(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47,-1.000)
!       call sum14568327(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47, 1.000)
!       call sum34578126(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47, 1.000)
!       call sum24578136(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47,-1.000)
!       call sum34578216(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47,-1.000)
!       call sum24578316(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47, 1.000)
!       call sum14578236(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47, 1.000)
!       call sum14578326(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47,-1.000)
!       call sum34678125(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47,-1.000)
!       call sum24678135(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47, 1.000)
!       call sum34678215(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47, 1.000)
!       call sum24678315(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47,-1.000)
!       call sum14678235(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47,-1.000)
!       call sum14678325(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z47, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z47(b,a,l,k,i,d,c,j)       ! 34568127 (-1.000)
!     & +z47(c,a,l,k,i,d,b,j)       ! 24568137 (+1.000)
!     & +z47(b,a,l,k,i,c,d,j)       ! 34568217 (+1.000)
!     & -z47(c,a,l,k,i,b,d,j)       ! 24568317 (-1.000)
!     & -z47(d,a,l,k,i,c,b,j)       ! 14568237 (-1.000)
!     & +z47(d,a,l,k,i,b,c,j)       ! 14568327 (+1.000)
!     & +z47(b,a,l,j,i,d,c,k)       ! 34578126 (+1.000)
!     & -z47(c,a,l,j,i,d,b,k)       ! 24578136 (-1.000)
!     & -z47(b,a,l,j,i,c,d,k)       ! 34578216 (-1.000)
!     & +z47(c,a,l,j,i,b,d,k)       ! 24578316 (+1.000)
!     & +z47(d,a,l,j,i,c,b,k)       ! 14578236 (+1.000)
!     & -z47(d,a,l,j,i,b,c,k)       ! 14578326 (-1.000)
!     & -z47(b,a,k,j,i,d,c,l)       ! 34678125 (-1.000)
!     & +z47(c,a,k,j,i,d,b,l)       ! 24678135 (+1.000)
!     & +z47(b,a,k,j,i,c,d,l)       ! 34678215 (+1.000)
!     & -z47(c,a,k,j,i,b,d,l)       ! 24678315 (-1.000)
!     & -z47(d,a,k,j,i,c,b,l)       ! 14678235 (-1.000)
!     & +z47(d,a,k,j,i,b,c,l)       ! 14678325 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z47)
       deallocate(x26)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3412(n2,n3,n0,n2,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,s75,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s149(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s149)
       deallocate(d1)
       deallocate(b2)
       deallocate(s75)
c
       call sum4123(n0,n2,n2,n3,n0,n2,n0,n2,x25,s149,-1.000)
       deallocate(s149)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
!       allocate(z46(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k1*k2*k3*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x25,f2,z46)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + x25(m,d,k,j)*t3c(c,b,a,m,l,i)          !mdkjcbamli      (+1.000)
     &     - x25(m,c,k,j)*t3c(d,b,a,m,l,i)          !mckjdbamli      (-1.000)
     &     + x25(m,b,k,j)*t3c(d,c,a,m,l,i)          !mbkjdcamli      (+1.000)
     &     - x25(m,d,l,j)*t3c(c,b,a,m,k,i)          !mdljcbamki      (-1.000)
     &     + x25(m,c,l,j)*t3c(d,b,a,m,k,i)          !mcljdbamki      (+1.000)
     &     - x25(m,b,l,j)*t3c(d,c,a,m,k,i)          !mbljdcamki      (-1.000)
     &     - x25(m,d,j,k)*t3c(c,b,a,m,l,i)          !mdjkcbamli      (-1.000)
     &     + x25(m,c,j,k)*t3c(d,b,a,m,l,i)          !mcjkdbamli      (+1.000)
     &     - x25(m,b,j,k)*t3c(d,c,a,m,l,i)          !mbjkdcamli      (-1.000)
     &     + x25(m,d,j,l)*t3c(c,b,a,m,k,i)          !mdjlcbamki      (+1.000)
     &     - x25(m,c,j,l)*t3c(d,b,a,m,k,i)          !mcjldbamki      (-1.000)
     &     + x25(m,b,j,l)*t3c(d,c,a,m,k,i)          !mbjldcamki      (+1.000)
     &     + x25(m,d,l,k)*t3c(c,b,a,m,j,i)          !mdlkcbamji      (+1.000)
     &     - x25(m,c,l,k)*t3c(d,b,a,m,j,i)          !mclkdbamji      (-1.000)
     &     + x25(m,b,l,k)*t3c(d,c,a,m,j,i)          !mblkdcamji      (+1.000)
     &     - x25(m,d,k,l)*t3c(c,b,a,m,j,i)          !mdklcbamji      (-1.000)
     &     + x25(m,c,k,l)*t3c(d,b,a,m,j,i)          !mckldbamji      (+1.000)
     &     - x25(m,b,k,l)*t3c(d,c,a,m,j,i)          !mbkldcamji      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23458167(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46, 1.000)
!       call sum13458267(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46,-1.000)
!       call sum12458367(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46, 1.000)
!       call sum23468157(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46,-1.000)
!       call sum13468257(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46, 1.000)
!       call sum12468357(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46,-1.000)
!       call sum23458176(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46,-1.000)
!       call sum13458276(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46, 1.000)
!       call sum12458376(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46,-1.000)
!       call sum23468175(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46, 1.000)
!       call sum13468275(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46,-1.000)
!       call sum12468375(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46, 1.000)
!       call sum23478156(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46, 1.000)
!       call sum13478256(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46,-1.000)
!       call sum12478356(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46, 1.000)
!       call sum23478165(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46,-1.000)
!       call sum13478265(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46, 1.000)
!       call sum12478365(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z46,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z46(c,b,a,l,i,d,k,j)       ! 23458167 (+1.000)
!     & -z46(d,b,a,l,i,c,k,j)       ! 13458267 (-1.000)
!     & +z46(d,c,a,l,i,b,k,j)       ! 12458367 (+1.000)
!     & -z46(c,b,a,k,i,d,l,j)       ! 23468157 (-1.000)
!     & +z46(d,b,a,k,i,c,l,j)       ! 13468257 (+1.000)
!     & -z46(d,c,a,k,i,b,l,j)       ! 12468357 (-1.000)
!     & -z46(c,b,a,l,i,d,j,k)       ! 23458176 (-1.000)
!     & +z46(d,b,a,l,i,c,j,k)       ! 13458276 (+1.000)
!     & -z46(d,c,a,l,i,b,j,k)       ! 12458376 (-1.000)
!     & +z46(c,b,a,k,i,d,j,l)       ! 23468175 (+1.000)
!     & -z46(d,b,a,k,i,c,j,l)       ! 13468275 (-1.000)
!     & +z46(d,c,a,k,i,b,j,l)       ! 12468375 (+1.000)
!     & +z46(c,b,a,j,i,d,l,k)       ! 23478156 (+1.000)
!     & -z46(d,b,a,j,i,c,l,k)       ! 13478256 (-1.000)
!     & +z46(d,c,a,j,i,b,l,k)       ! 12478356 (+1.000)
!     & -z46(c,b,a,j,i,d,k,l)       ! 23478165 (-1.000)
!     & +z46(d,b,a,j,i,c,k,l)       ! 13478265 (+1.000)
!     & -z46(d,c,a,j,i,b,k,l)       ! 12478365 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z46)
       deallocate(x25)
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
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder4312(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
       allocate(s76(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k4*k4
       i3=k2*k2
       call egemm(i1,i2,i3,d1,d2,s76)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x19(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       x19=0.0d0
       call sum3412(n2,n3,n2,n3,n2,n3,n2,n3,x19,s76, 0.500)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder4312(n2,n3,n2,n3,n2,n3,n2,n3,
     & n2,n3,n2,n3,n2,n3,n2,n3,s76,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s150(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4*k4
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s150)
       deallocate(d1)
       deallocate(b2)
       deallocate(s76)
c
       call sum4123(n2,n3,n2,n3,n2,n3,n0,n2,x4,s150, 0.500)
       deallocate(s150)
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
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder251346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(u47(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k2*k3*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u47)
       deallocate(d1)
       deallocate(f2)
c
      call sum345621(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,x31,u47, 1.000)
       deallocate(u47)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder235146(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n2,n3,n0,n2,n0,n1,t3b,f2)
       allocate(s77(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k2*k4
       i3=k1*k3*k3
       call egemm(i1,i2,i3,d1,f2,s77)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s77,-0.500)
       deallocate(s77)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n1,n3,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder265134(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n2,n3,n1,n3,n0,n2,t3b,f2)
       allocate(s78(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
       i1=k3
       i2=k2*k3*k4
       i3=k1*k1*k3
       call egemm(i1,i2,i3,d1,f2,s78)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x2,s78, 0.500)
       deallocate(s78)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder251346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(u48(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k2*k3*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u48)
       deallocate(d1)
       deallocate(f2)
c
      call sum345621(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x36,u48, 1.000)
c
       allocate(f1(n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder561234(n2,n3,n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,u48,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u91(n0+1:n2,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u91)
       deallocate(f1)
       deallocate(b2)
       deallocate(u48)
c
      call sum512346(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x40,u91, 1.000)
       deallocate(u91)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z135(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x40,d2,z135)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     + x40(n,b,a,l,j,i)*t2c(d,c,n,k)          !nbaljidcnk      (+1.000)
     &     - x40(n,c,a,l,j,i)*t2c(d,b,n,k)          !ncaljidbnk      (-1.000)
     &     + x40(n,d,a,l,j,i)*t2c(c,b,n,k)          !ndaljicbnk      (+1.000)
     &     - x40(n,b,a,k,j,i)*t2c(d,c,n,l)          !nbakjidcnl      (-1.000)
     &     + x40(n,c,a,k,j,i)*t2c(d,b,n,l)          !ncakjidbnl      (+1.000)
     &     - x40(n,d,a,k,j,i)*t2c(c,b,n,l)          !ndakjicbnl      (-1.000)
     &     - x40(n,b,a,l,k,i)*t2c(d,c,n,j)          !nbalkidcnj      (-1.000)
     &     + x40(n,c,a,l,k,i)*t2c(d,b,n,j)          !ncalkidbnj      (+1.000)
     &     - x40(n,d,a,l,k,i)*t2c(c,b,n,j)          !ndalkicbnj      (-1.000)
     &     + x40(n,b,a,k,l,i)*t2c(d,c,n,j)          !nbaklidcnj      (+1.000)
     &     - x40(n,c,a,k,l,i)*t2c(d,b,n,j)          !ncaklidbnj      (-1.000)
     &     + x40(n,d,a,k,l,i)*t2c(c,b,n,j)          !ndaklicbnj      (+1.000)
     &     + x40(n,b,a,j,k,i)*t2c(d,c,n,l)          !nbajkidcnl      (+1.000)
     &     - x40(n,c,a,j,k,i)*t2c(d,b,n,l)          !ncajkidbnl      (-1.000)
     &     + x40(n,d,a,j,k,i)*t2c(c,b,n,l)          !ndajkicbnl      (+1.000)
     &     - x40(n,b,a,j,l,i)*t2c(d,c,n,k)          !nbajlidcnk      (-1.000)
     &     + x40(n,c,a,j,l,i)*t2c(d,b,n,k)          !ncajlidbnk      (+1.000)
     &     - x40(n,d,a,j,l,i)*t2c(c,b,n,k)          !ndajlicbnk      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12634578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135, 1.000)
!       call sum13624578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135,-1.000)
!       call sum23614578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135, 1.000)
!       call sum12534678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135,-1.000)
!       call sum13524678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135, 1.000)
!       call sum23514678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135,-1.000)
!       call sum12734568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135,-1.000)
!       call sum13724568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135, 1.000)
!       call sum23714568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135,-1.000)
!       call sum12734658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135, 1.000)
!       call sum13724658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135,-1.000)
!       call sum23714658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135, 1.000)
!       call sum12534768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135, 1.000)
!       call sum13524768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135,-1.000)
!       call sum23514768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135, 1.000)
!       call sum12634758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135,-1.000)
!       call sum13624758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135, 1.000)
!       call sum23614758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z135,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z135(d,c,k,b,a,l,j,i)       ! 12634578 (+1.000)
!     & -z135(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!     & +z135(c,b,k,d,a,l,j,i)       ! 23614578 (+1.000)
!     & -z135(d,c,l,b,a,k,j,i)       ! 12534678 (-1.000)
!     & +z135(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & -z135(c,b,l,d,a,k,j,i)       ! 23514678 (-1.000)
!     & -z135(d,c,j,b,a,l,k,i)       ! 12734568 (-1.000)
!     & +z135(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!     & -z135(c,b,j,d,a,l,k,i)       ! 23714568 (-1.000)
!     & +z135(d,c,j,b,a,k,l,i)       ! 12734658 (+1.000)
!     & -z135(d,b,j,c,a,k,l,i)       ! 13724658 (-1.000)
!     & +z135(c,b,j,d,a,k,l,i)       ! 23714658 (+1.000)
!     & +z135(d,c,l,b,a,j,k,i)       ! 12534768 (+1.000)
!     & -z135(d,b,l,c,a,j,k,i)       ! 13524768 (-1.000)
!     & +z135(c,b,l,d,a,j,k,i)       ! 23514768 (+1.000)
!     & -z135(d,c,k,b,a,j,l,i)       ! 12634758 (-1.000)
!     & +z135(d,b,k,c,a,j,l,i)       ! 13624758 (+1.000)
!     & -z135(c,b,k,d,a,j,l,i)       ! 23614758 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z135)
       deallocate(x40)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder125346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n1,n3,n0,n1,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(s79(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k2*k3
       i3=k1*k3*k4
       call egemm(i1,i2,i3,d1,f2,s79)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x5,s79, 1.000)
       deallocate(s79)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n2,n2,n3,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder254136(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n2,n2,n3,n1,n3,n0,n1,t3b,f2)
       allocate(s80(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k3*k4
       i3=k2*k1*k3
       call egemm(i1,i2,i3,d1,f2,s80)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n2,n3,n2,n3,n1,n3,n0,n1,x6,s80,-1.000)
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
!       allocate(d1(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n1,n3,
!     & n1,n3,n2,n3,n1,n3,n0,n2,s81,d1)
!       allocate(f2(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder312456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
!       allocate(z169(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n1+1:n3,n0+1:n2))
!       i1=k2*k3*k4
!       i2=k1*k2*k2*k4*k4
!       i3=k3
!       call egemm(i1,i2,i3,d1,f2,z169)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     - s81(b,a,l,e)*t3c(d,c,e,k,j,i)          !baledcekji      (-1.000)
     &     + s81(c,a,l,e)*t3c(d,b,e,k,j,i)          !caledbekji      (+1.000)
     &     - s81(d,a,l,e)*t3c(c,b,e,k,j,i)          !dalecbekji      (-1.000)
     &     + s81(b,a,k,e)*t3c(d,c,e,l,j,i)          !bakedcelji      (+1.000)
     &     - s81(c,a,k,e)*t3c(d,b,e,l,j,i)          !cakedbelji      (-1.000)
     &     + s81(d,a,k,e)*t3c(c,b,e,l,j,i)          !dakecbelji      (+1.000)
     &     - s81(b,a,j,e)*t3c(d,c,e,l,k,i)          !bajedcelki      (-1.000)
     &     + s81(c,a,j,e)*t3c(d,b,e,l,k,i)          !cajedbelki      (+1.000)
     &     - s81(d,a,j,e)*t3c(c,b,e,l,k,i)          !dajecbelki      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12678345(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z169,-1.000)
!       call sum13678245(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z169, 1.000)
!       call sum23678145(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z169,-1.000)
!       call sum12578346(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z169, 1.000)
!       call sum13578246(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z169,-1.000)
!       call sum23578146(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z169, 1.000)
!       call sum12568347(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z169,-1.000)
!       call sum13568247(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z169, 1.000)
!       call sum23568147(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z169,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z169(d,c,k,j,i,b,a,l)       ! 12678345 (-1.000)
!     & +z169(d,b,k,j,i,c,a,l)       ! 13678245 (+1.000)
!     & -z169(c,b,k,j,i,d,a,l)       ! 23678145 (-1.000)
!     & +z169(d,c,l,j,i,b,a,k)       ! 12578346 (+1.000)
!     & -z169(d,b,l,j,i,c,a,k)       ! 13578246 (-1.000)
!     & +z169(c,b,l,j,i,d,a,k)       ! 23578146 (+1.000)
!     & -z169(d,c,l,k,i,b,a,j)       ! 12568347 (-1.000)
!     & +z169(d,b,l,k,i,c,a,j)       ! 13568247 (+1.000)
!     & -z169(c,b,l,k,i,d,a,j)       ! 23568147 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z169)
       deallocate(s81)
c
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n2,n2,n3,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder364125(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n1,n3,n0,n1,n0,n2,n2,n3,n2,n3,n0,n2,t3c,f2)
       allocate(s82(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
       i1=k4
       i2=k2*k4*k4
       i3=k2*k1*k3
       call egemm(i1,i2,i3,d1,f2,s82)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n2,n3,n0,n2,n2,n3,
!     & n2,n3,n2,n3,n2,n3,n0,n2,s82,d1)
!       allocate(f2(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder123456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
!       allocate(z170(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k1*k2*k2*k3*k4
!       i3=k4
!       call egemm(i1,i2,i3,d1,f2,z170)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum
     &     + s82(c,b,l,f)*t3c(f,d,a,k,j,i)          !cblffdakji      (+1.000)
     &     - s82(d,b,l,f)*t3c(f,c,a,k,j,i)          !dblffcakji      (-1.000)
     &     + s82(d,c,l,f)*t3c(f,b,a,k,j,i)          !dclffbakji      (+1.000)
     &     - s82(c,b,k,f)*t3c(f,d,a,l,j,i)          !cbkffdalji      (-1.000)
     &     + s82(d,b,k,f)*t3c(f,c,a,l,j,i)          !dbkffcalji      (+1.000)
     &     - s82(d,c,k,f)*t3c(f,b,a,l,j,i)          !dckffbalji      (-1.000)
     &     + s82(c,b,j,f)*t3c(f,d,a,l,k,i)          !cbjffdalki      (+1.000)
     &     - s82(d,b,j,f)*t3c(f,c,a,l,k,i)          !dbjffcalki      (-1.000)
     &     + s82(d,c,j,f)*t3c(f,b,a,l,k,i)          !dcjffbalki      (+1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14678235(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z170, 1.000)
!       call sum24678135(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z170,-1.000)
!       call sum34678125(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z170, 1.000)
!       call sum14578236(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z170,-1.000)
!       call sum24578136(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z170, 1.000)
!       call sum34578126(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z170,-1.000)
!       call sum14568237(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z170, 1.000)
!       call sum24568137(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z170,-1.000)
!       call sum34568127(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z170, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z170(d,a,k,j,i,c,b,l)       ! 14678235 (+1.000)
!     & -z170(c,a,k,j,i,d,b,l)       ! 24678135 (-1.000)
!     & +z170(b,a,k,j,i,d,c,l)       ! 34678125 (+1.000)
!     & -z170(d,a,l,j,i,c,b,k)       ! 14578236 (-1.000)
!     & +z170(c,a,l,j,i,d,b,k)       ! 24578136 (+1.000)
!     & -z170(b,a,l,j,i,d,c,k)       ! 34578126 (-1.000)
!     & +z170(d,a,l,k,i,c,b,j)       ! 14568237 (+1.000)
!     & -z170(c,a,l,k,i,d,b,j)       ! 24568137 (-1.000)
!     & +z170(b,a,l,k,i,d,c,j)       ! 34568127 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z170)
       deallocate(s82)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder132456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n1,n3,n2,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
       allocate(u49(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k2*k2*k4
       i3=k3*k4
       call egemm(i1,i2,i3,d1,f2,u49)
       deallocate(d1)
       deallocate(f2)
c
      call sum345621(n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,x42,u49, 1.000)
c
!       allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder461235(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,t3c,f2)
!       allocate(z139(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k4
!       i2=k2*k3*k4*k4
!       i3=k1*k2
!       call egemm(i1,i2,i3,x42,f2,z139)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     - x42(n,m,d,k,j,i)*t3c(c,b,a,n,l,m)      !nmdkjicbanlm    (-1.000)
     &     + x42(n,m,c,k,j,i)*t3c(d,b,a,n,l,m)      !nmckjidbanlm    (+1.000)
     &     - x42(n,m,b,k,j,i)*t3c(d,c,a,n,l,m)      !nmbkjidcanlm    (-1.000)
     &     + x42(n,m,d,l,j,i)*t3c(c,b,a,n,k,m)      !nmdljicbankm    (+1.000)
     &     - x42(n,m,c,l,j,i)*t3c(d,b,a,n,k,m)      !nmcljidbankm    (-1.000)
     &     + x42(n,m,b,l,j,i)*t3c(d,c,a,n,k,m)      !nmbljidcankm    (+1.000)
     &     - x42(n,m,d,l,k,i)*t3c(c,b,a,n,j,m)      !nmdlkicbanjm    (-1.000)
     &     + x42(n,m,c,l,k,i)*t3c(d,b,a,n,j,m)      !nmclkidbanjm    (+1.000)
     &     - x42(n,m,b,l,k,i)*t3c(d,c,a,n,j,m)      !nmblkidcanjm    (-1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23451678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z139,-1.000)
!       call sum13452678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z139, 1.000)
!       call sum12453678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z139,-1.000)
!       call sum23461578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z139, 1.000)
!       call sum13462578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z139,-1.000)
!       call sum12463578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z139, 1.000)
!       call sum23471568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z139,-1.000)
!       call sum13472568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z139, 1.000)
!       call sum12473568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z139,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z139(c,b,a,l,d,k,j,i)       ! 23451678 (-1.000)
!     & +z139(d,b,a,l,c,k,j,i)       ! 13452678 (+1.000)
!     & -z139(d,c,a,l,b,k,j,i)       ! 12453678 (-1.000)
!     & +z139(c,b,a,k,d,l,j,i)       ! 23461578 (+1.000)
!     & -z139(d,b,a,k,c,l,j,i)       ! 13462578 (-1.000)
!     & +z139(d,c,a,k,b,l,j,i)       ! 12463578 (+1.000)
!     & -z139(c,b,a,j,d,l,k,i)       ! 23471568 (-1.000)
!     & +z139(d,b,a,j,c,l,k,i)       ! 13472568 (+1.000)
!     & -z139(d,c,a,j,b,l,k,i)       ! 12473568 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z139)
       deallocate(x42)
c
       allocate(f1(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder651234(n2,n3,n0,n2,n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,u49,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u86(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u86)
       deallocate(f1)
       deallocate(b2)
c
!!!!!!!!the corresponding term u82(u51) in t4b was explicitly changed.
      call sum213456(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x22,u86,-1.000)
       deallocate(u86)
c
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z206(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k4*k4
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x22,d2,z206)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x22(m,d,b,l,k,i)*t2b(c,a,j,m)          !mdblkicajm      (+1.000)
     &     - x22(m,d,c,l,k,i)*t2b(b,a,j,m)          !mdclkibajm      (-1.000)
     &     - x22(m,c,b,l,k,i)*t2b(d,a,j,m)          !mcblkidajm      (-1.000)
     &     + x22(m,b,c,l,k,i)*t2b(d,a,j,m)          !mbclkidajm      (+1.000)
     &     + x22(m,c,d,l,k,i)*t2b(b,a,j,m)          !mcdlkibajm      (+1.000)
     &     - x22(m,b,d,l,k,i)*t2b(c,a,j,m)          !mbdlkicajm      (-1.000)
     &     - x22(m,d,b,l,j,i)*t2b(c,a,k,m)          !mdbljicakm      (-1.000)
     &     + x22(m,d,c,l,j,i)*t2b(b,a,k,m)          !mdcljibakm      (+1.000)
     &     + x22(m,c,b,l,j,i)*t2b(d,a,k,m)          !mcbljidakm      (+1.000)
     &     - x22(m,b,c,l,j,i)*t2b(d,a,k,m)          !mbcljidakm      (-1.000)
     &     - x22(m,c,d,l,j,i)*t2b(b,a,k,m)          !mcdljibakm      (-1.000)
     &     + x22(m,b,d,l,j,i)*t2b(c,a,k,m)          !mbdljicakm      (+1.000)
     &     + x22(m,d,b,k,j,i)*t2b(c,a,l,m)          !mdbkjicalm      (+1.000)
     &     - x22(m,d,c,k,j,i)*t2b(b,a,l,m)          !mdckjibalm      (-1.000)
     &     - x22(m,c,b,k,j,i)*t2b(d,a,l,m)          !mcbkjidalm      (-1.000)
     &     + x22(m,b,c,k,j,i)*t2b(d,a,l,m)          !mbckjidalm      (+1.000)
     &     + x22(m,c,d,k,j,i)*t2b(b,a,l,m)          !mcdkjibalm      (+1.000)
     &     - x22(m,b,d,k,j,i)*t2b(c,a,l,m)          !mbdkjicalm      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum24713568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206, 1.000)
!       call sum34712568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206,-1.000)
!       call sum14723568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206,-1.000)
!       call sum14732568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206, 1.000)
!       call sum34721568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206, 1.000)
!       call sum24731568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206,-1.000)
!       call sum24613578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206,-1.000)
!       call sum34612578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206, 1.000)
!       call sum14623578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206, 1.000)
!       call sum14632578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206,-1.000)
!       call sum34621578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206,-1.000)
!       call sum24631578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206, 1.000)
!       call sum24513678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206, 1.000)
!       call sum34512678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206,-1.000)
!       call sum14523678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206,-1.000)
!       call sum14532678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206, 1.000)
!       call sum34521678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206, 1.000)
!       call sum24531678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z206,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z206(c,a,j,d,b,l,k,i)       ! 24713568 (+1.000)
!     & -z206(b,a,j,d,c,l,k,i)       ! 34712568 (-1.000)
!     & -z206(d,a,j,c,b,l,k,i)       ! 14723568 (-1.000)
!     & +z206(d,a,j,b,c,l,k,i)       ! 14732568 (+1.000)
!     & +z206(b,a,j,c,d,l,k,i)       ! 34721568 (+1.000)
!     & -z206(c,a,j,b,d,l,k,i)       ! 24731568 (-1.000)
!     & -z206(c,a,k,d,b,l,j,i)       ! 24613578 (-1.000)
!     & +z206(b,a,k,d,c,l,j,i)       ! 34612578 (+1.000)
!     & +z206(d,a,k,c,b,l,j,i)       ! 14623578 (+1.000)
!     & -z206(d,a,k,b,c,l,j,i)       ! 14632578 (-1.000)
!     & -z206(b,a,k,c,d,l,j,i)       ! 34621578 (-1.000)
!     & +z206(c,a,k,b,d,l,j,i)       ! 24631578 (+1.000)
!     & +z206(c,a,l,d,b,k,j,i)       ! 24513678 (+1.000)
!     & -z206(b,a,l,d,c,k,j,i)       ! 34512678 (-1.000)
!     & -z206(d,a,l,c,b,k,j,i)       ! 14523678 (-1.000)
!     & +z206(d,a,l,b,c,k,j,i)       ! 14532678 (+1.000)
!     & +z206(b,a,l,c,d,k,j,i)       ! 34521678 (+1.000)
!     & -z206(c,a,l,b,d,k,j,i)       ! 24531678 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z206)
       deallocate(x22)
c
       allocate(f1(n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder561234(n2,n3,n0,n2,n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n2,n3,n0,n2,n0,n2,n0,n1,u49,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u68(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2*k4*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u68)
       deallocate(f1)
       deallocate(b2)
       deallocate(u49)
c
      call sum312456(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x43,u68, 1.000)
       deallocate(u68)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder136245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n1,n2,n3,n0,n2,n0,n2,t3c,f2)
       allocate(s83(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2*k2*k4
       i3=k1*k3*k4
       call egemm(i1,i2,i3,d1,f2,s83)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder4123(n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n0,n2,n0,n2,s83,d1)
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
!       allocate(z172(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k1*k2*k3*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,d1,f2,z172)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     + s83(b,l,k,n)*t3c(d,c,a,n,j,i)          !blkndcanji      (+1.000)
     &     - s83(c,l,k,n)*t3c(d,b,a,n,j,i)          !clkndbanji      (-1.000)
     &     + s83(d,l,k,n)*t3c(c,b,a,n,j,i)          !dlkncbanji      (+1.000)
     &     - s83(b,l,j,n)*t3c(d,c,a,n,k,i)          !bljndcanki      (-1.000)
     &     + s83(c,l,j,n)*t3c(d,b,a,n,k,i)          !cljndbanki      (+1.000)
     &     - s83(d,l,j,n)*t3c(c,b,a,n,k,i)          !dljncbanki      (-1.000)
     &     + s83(b,k,j,n)*t3c(d,c,a,n,l,i)          !bkjndcanli      (+1.000)
     &     - s83(c,k,j,n)*t3c(d,b,a,n,l,i)          !ckjndbanli      (-1.000)
     &     + s83(d,k,j,n)*t3c(c,b,a,n,l,i)          !dkjncbanli      (+1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12478356(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z172, 1.000)
!       call sum13478256(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z172,-1.000)
!       call sum23478156(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z172, 1.000)
!       call sum12468357(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z172,-1.000)
!       call sum13468257(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z172, 1.000)
!       call sum23468157(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z172,-1.000)
!       call sum12458367(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z172, 1.000)
!       call sum13458267(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z172,-1.000)
!       call sum23458167(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z172, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z172(d,c,a,j,i,b,l,k)       ! 12478356 (+1.000)
!     & -z172(d,b,a,j,i,c,l,k)       ! 13478256 (-1.000)
!     & +z172(c,b,a,j,i,d,l,k)       ! 23478156 (+1.000)
!     & -z172(d,c,a,k,i,b,l,j)       ! 12468357 (-1.000)
!     & +z172(d,b,a,k,i,c,l,j)       ! 13468257 (+1.000)
!     & -z172(c,b,a,k,i,d,l,j)       ! 23468157 (-1.000)
!     & +z172(d,c,a,l,i,b,k,j)       ! 12458367 (+1.000)
!     & -z172(d,b,a,l,i,c,k,j)       ! 13458267 (-1.000)
!     & +z172(c,b,a,l,i,d,k,j)       ! 23458167 (+1.000)
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
       deallocate(s83)
c
       allocate(d1(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n1,n3,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder162345(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n0,n1,n2,n3,n1,n3,n0,n2,n0,n2,t3c,f2)
       allocate(u50(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k2*k2*k3*k4
       i3=k1*k4
       call egemm(i1,i2,i3,d1,f2,u50)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
!       call reorder651234(n2,n3,n1,n3,n0,n2,n0,n2,n1,n3,n0,n2,
!     & n0,n2,n1,n3,n2,n3,n1,n3,n0,n2,n0,n2,u50,f1)
!       allocate(f2(n0+1:n2,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder431256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n1,n3,n2,n3,n2,n3,n0,n2,n0,n1,t3c,f2)
!       allocate(z173(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k3*k4
!       i2=k1*k2*k4*k4
!       i3=k3*k2
!       call egemm(i1,i2,i3,f1,f2,z173)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do n=n0+1,n2
             sum=sum
     &     - u50(b,a,l,k,e,n)*t3c(d,c,e,n,j,i)      !balkendcenji    (-1.000)
     &     + u50(c,a,l,k,e,n)*t3c(d,b,e,n,j,i)      !calkendbenji    (+1.000)
     &     - u50(d,a,l,k,e,n)*t3c(c,b,e,n,j,i)      !dalkencbenji    (-1.000)
     &     + u50(b,a,l,j,e,n)*t3c(d,c,e,n,k,i)      !baljendcenki    (+1.000)
     &     - u50(c,a,l,j,e,n)*t3c(d,b,e,n,k,i)      !caljendbenki    (-1.000)
     &     + u50(d,a,l,j,e,n)*t3c(c,b,e,n,k,i)      !daljencbenki    (+1.000)
     &     - u50(b,a,k,j,e,n)*t3c(d,c,e,n,l,i)      !bakjendcenli    (-1.000)
     &     + u50(c,a,k,j,e,n)*t3c(d,b,e,n,l,i)      !cakjendbenli    (+1.000)
     &     - u50(d,a,k,j,e,n)*t3c(c,b,e,n,l,i)      !dakjencbenli    (-1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12783456(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z173,-1.000)
!       call sum13782456(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z173, 1.000)
!       call sum23781456(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z173,-1.000)
!       call sum12683457(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z173, 1.000)
!       call sum13682457(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z173,-1.000)
!       call sum23681457(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z173, 1.000)
!       call sum12583467(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z173,-1.000)
!       call sum13582467(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z173, 1.000)
!       call sum23581467(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z173,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z173(d,c,j,i,b,a,l,k)       ! 12783456 (-1.000)
!     & +z173(d,b,j,i,c,a,l,k)       ! 13782456 (+1.000)
!     & -z173(c,b,j,i,d,a,l,k)       ! 23781456 (-1.000)
!     & +z173(d,c,k,i,b,a,l,j)       ! 12683457 (+1.000)
!     & -z173(d,b,k,i,c,a,l,j)       ! 13682457 (-1.000)
!     & +z173(c,b,k,i,d,a,l,j)       ! 23681457 (+1.000)
!     & -z173(d,c,l,i,b,a,k,j)       ! 12583467 (-1.000)
!     & +z173(d,b,l,i,c,a,k,j)       ! 13582467 (+1.000)
!     & -z173(c,b,l,i,d,a,k,j)       ! 23581467 (-1.000)
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
c
       allocate(f1(n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder561234(n2,n3,n1,n3,n0,n2,n0,n2,n1,n3,n0,n2,
     & n1,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,u50,f1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(u66(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k3*k4*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,f1,b2,u66)
       deallocate(f1)
       deallocate(b2)
       deallocate(u50)
c
      call sum612345(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x43,u66, 1.000)
       deallocate(u66)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z140(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x43,d2,z140)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - x43(n,b,a,l,k,i)*t2c(d,c,n,j)          !nbalkidcnj      (-1.000)
     &     + x43(n,c,a,l,k,i)*t2c(d,b,n,j)          !ncalkidbnj      (+1.000)
     &     - x43(n,d,a,l,k,i)*t2c(c,b,n,j)          !ndalkicbnj      (-1.000)
     &     + x43(n,b,a,l,j,i)*t2c(d,c,n,k)          !nbaljidcnk      (+1.000)
     &     - x43(n,c,a,l,j,i)*t2c(d,b,n,k)          !ncaljidbnk      (-1.000)
     &     + x43(n,d,a,l,j,i)*t2c(c,b,n,k)          !ndaljicbnk      (+1.000)
     &     - x43(n,b,a,k,j,i)*t2c(d,c,n,l)          !nbakjidcnl      (-1.000)
     &     + x43(n,c,a,k,j,i)*t2c(d,b,n,l)          !ncakjidbnl      (+1.000)
     &     - x43(n,d,a,k,j,i)*t2c(c,b,n,l)          !ndakjicbnl      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12734568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z140,-1.000)
!       call sum13724568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z140, 1.000)
!       call sum23714568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z140,-1.000)
!       call sum12634578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z140, 1.000)
!       call sum13624578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z140,-1.000)
!       call sum23614578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z140, 1.000)
!       call sum12534678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z140,-1.000)
!       call sum13524678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z140, 1.000)
!       call sum23514678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z140,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z140(d,c,j,b,a,l,k,i)       ! 12734568 (-1.000)
!     & +z140(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!     & -z140(c,b,j,d,a,l,k,i)       ! 23714568 (-1.000)
!     & +z140(d,c,k,b,a,l,j,i)       ! 12634578 (+1.000)
!     & -z140(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!     & +z140(c,b,k,d,a,l,j,i)       ! 23614578 (+1.000)
!     & -z140(d,c,l,b,a,k,j,i)       ! 12534678 (-1.000)
!     & +z140(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & -z140(c,b,l,d,a,k,j,i)       ! 23514678 (-1.000)
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
       deallocate(x43)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder142356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
       allocate(u51(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k2*k3*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u51)
       deallocate(d1)
       deallocate(f2)
c
      call sum345621(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,x31,u51, 1.000)
       deallocate(u51)
c
!       allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder631245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
!       allocate(z86(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k4
!       i2=k2*k2*k4*k4
!       i3=k3*k1
!       call egemm(i1,i2,i3,x31,f2,z86)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + x31(m,f,d,a,j,i)*t3c(c,b,f,l,k,m)      !mfdajicbflkm    (+1.000)
     &     - x31(m,f,c,a,j,i)*t3c(d,b,f,l,k,m)      !mfcajidbflkm    (-1.000)
     &     + x31(m,f,b,a,j,i)*t3c(d,c,f,l,k,m)      !mfbajidcflkm    (+1.000)
     &     - x31(m,f,d,a,k,i)*t3c(c,b,f,l,j,m)      !mfdakicbfljm    (-1.000)
     &     + x31(m,f,c,a,k,i)*t3c(d,b,f,l,j,m)      !mfcakidbfljm    (+1.000)
     &     - x31(m,f,b,a,k,i)*t3c(d,c,f,l,j,m)      !mfbakidcfljm    (-1.000)
     &     + x31(m,f,d,a,l,i)*t3c(c,b,f,k,j,m)      !mfdalicbfkjm    (+1.000)
     &     - x31(m,f,c,a,l,i)*t3c(d,b,f,k,j,m)      !mfcalidbfkjm    (-1.000)
     &     + x31(m,f,b,a,l,i)*t3c(d,c,f,k,j,m)      !mfbalidcfkjm    (+1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23561478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z86, 1.000)
!       call sum13562478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z86,-1.000)
!       call sum12563478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z86, 1.000)
!       call sum23571468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z86,-1.000)
!       call sum13572468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z86, 1.000)
!       call sum12573468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z86,-1.000)
!       call sum23671458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z86, 1.000)
!       call sum13672458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z86,-1.000)
!       call sum12673458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z86, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z86(c,b,l,k,d,a,j,i)       ! 23561478 (+1.000)
!     & -z86(d,b,l,k,c,a,j,i)       ! 13562478 (-1.000)
!     & +z86(d,c,l,k,b,a,j,i)       ! 12563478 (+1.000)
!     & -z86(c,b,l,j,d,a,k,i)       ! 23571468 (-1.000)
!     & +z86(d,b,l,j,c,a,k,i)       ! 13572468 (+1.000)
!     & -z86(d,c,l,j,b,a,k,i)       ! 12573468 (-1.000)
!     & +z86(c,b,k,j,d,a,l,i)       ! 23671458 (+1.000)
!     & -z86(d,b,k,j,c,a,l,i)       ! 13672458 (-1.000)
!     & +z86(d,c,k,j,b,a,l,i)       ! 12673458 (+1.000)
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
       deallocate(x31)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder134256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n2,n3,n0,n2,n0,n1,t3c,f2)
       allocate(s84(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k2*k4
       i3=k2*k3*k4
       call egemm(i1,i2,i3,d1,f2,s84)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s84, 1.000)
       deallocate(s84)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n2,n3,intb,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder154236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n2,n3,n0,n2,n0,n2,n2,n3,n2,n3,n0,n2,t3d,f2)
       allocate(s85(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
       i1=k4
       i2=k2*k4*k4
       i3=k2*k2*k4
       call egemm(i1,i2,i3,d1,f2,s85)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n2,n3,n0,n2,n2,n3,
!     & n2,n3,n2,n3,n2,n3,n0,n2,s85,d1)
!       allocate(f2(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder123456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
!       allocate(z176(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k1*k2*k2*k3*k4
!       i3=k4
!       call egemm(i1,i2,i3,d1,f2,z176)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     + (s85(c,b,l,e)*t3c(e,d,a,k,j,i)         !cbleedakji      (+0.500)
     &     - s85(d,b,l,e)*t3c(e,c,a,k,j,i)          !dbleecakji      (-0.500)
     &     + s85(d,c,l,e)*t3c(e,b,a,k,j,i)          !dcleebakji      (+0.500)
     &     - s85(c,b,k,e)*t3c(e,d,a,l,j,i)          !cbkeedalji      (-0.500)
     &     + s85(d,b,k,e)*t3c(e,c,a,l,j,i)          !dbkeecalji      (+0.500)
     &     - s85(d,c,k,e)*t3c(e,b,a,l,j,i)          !dckeebalji      (-0.500)
     &     + s85(c,b,j,e)*t3c(e,d,a,l,k,i)          !cbjeedalki      (+0.500)
     &     - s85(d,b,j,e)*t3c(e,c,a,l,k,i)          !dbjeecalki      (-0.500)
     &     + s85(d,c,j,e)*t3c(e,b,a,l,k,i))/2.0d0   !dcjeebalki      (+0.500)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14678235(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z176, 0.500)
!       call sum24678135(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z176,-0.500)
!       call sum34678125(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z176, 0.500)
!       call sum14578236(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z176,-0.500)
!       call sum24578136(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z176, 0.500)
!       call sum34578126(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z176,-0.500)
!       call sum14568237(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z176, 0.500)
!       call sum24568137(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z176,-0.500)
!       call sum34568127(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z176, 0.500)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z176(d,a,k,j,i,c,b,l)      ! 14678235 (+0.500)
!     & -z176(c,a,k,j,i,d,b,l)       ! 24678135 (-0.500)
!     & +z176(b,a,k,j,i,d,c,l)       ! 34678125 (+0.500)
!     & -z176(d,a,l,j,i,c,b,k)       ! 14578236 (-0.500)
!     & +z176(c,a,l,j,i,d,b,k)       ! 24578136 (+0.500)
!     & -z176(b,a,l,j,i,d,c,k)       ! 34578126 (-0.500)
!     & +z176(d,a,l,k,i,c,b,j)       ! 14568237 (+0.500)
!     & -z176(c,a,l,k,i,d,b,j)       ! 24568137 (-0.500)
!     & +z176(b,a,l,k,i,d,c,j))/2.0d0! 34568127 (+0.500)
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
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder123456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
       allocate(u52(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k2*k2*k3
       i3=k4*k4
       call egemm(i1,i2,i3,d1,f2,u52)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder651234(n1,n3,n0,n2,n0,n2,n0,n1,n0,n2,n0,n2,
!     & n0,n2,n0,n2,n1,n3,n0,n2,n0,n2,n0,n1,u52,f1)
!       allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder451236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,t3d,f2)
!       allocate(z177(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3
!       i2=k2*k4*k4*k4
!       i3=k2*k2
!       call egemm(i1,i2,i3,f1,f2,z177)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2;do n=n0+1,n2
             sum=sum
     &     + (u52(a,k,j,i,m,n)*t3d(d,c,b,n,m,l)       !akjimndcbnml    (+0.250)
     &     - u52(a,l,j,i,m,n)*t3d(d,c,b,n,m,k)        !aljimndcbnmk    (-0.250)
     &     + u52(a,l,k,i,m,n)*t3d(d,c,b,n,m,j))/4.0d0 !alkimndcbnmj    (+0.250)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12354678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z177, 0.250)
!       call sum12364578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z177,-0.250)
!       call sum12374568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z177, 0.250)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z177(d,c,b,l,a,k,j,i)      ! 12354678 (+0.250)
!     & -z177(d,c,b,k,a,l,j,i)       ! 12364578 (-0.250)
!     & +z177(d,c,b,j,a,l,k,i))/4.0d0! 12374568 (+0.250)
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
       allocate(f1(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder561234(n1,n3,n0,n2,n0,n2,n0,n1,n0,n2,n0,n2,
     & n0,n2,n0,n2,n1,n3,n0,n2,n0,n2,n0,n1,u52,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u96(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2*k3*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u96)
       deallocate(f1)
       deallocate(b2)
       deallocate(u52)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder213456(n2,n3,n0,n2,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,u96,f1)
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z290(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,f1,d2,z290)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum                                !top two switched
     &     + (u96(c,n,a,l,k,i)*t2c(d,b,n,j)         !cnalkidbnj      (+0.500)
     &     - u96(d,n,a,l,k,i)*t2c(c,b,n,j)          !dnalkicbnj      (-0.500)
     &     - u96(b,n,a,l,k,i)*t2c(d,c,n,j)          !bnalkidcnj      (-0.500)
     &     + u96(d,n,a,l,j,i)*t2c(c,b,n,k)          !dnaljicbnk      (+0.500)
     &     - u96(c,n,a,l,j,i)*t2c(d,b,n,k)          !cnaljidbnk      (-0.500)
     &     + u96(b,n,a,l,j,i)*t2c(d,c,n,k)          !bnaljidcnk      (+0.500)
     &     - u96(d,n,a,k,j,i)*t2c(c,b,n,l)          !dnakjicbnl      (-0.500)
     &     + u96(c,n,a,k,j,i)*t2c(d,b,n,l)          !cnakjidbnl      (+0.500)
     &     - u96(b,n,a,k,j,i)*t2c(d,c,n,l))/2.0d0   !bnakjidcnl      (-0.500)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23714568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z290,-0.500)
!       call sum13724568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z290, 0.500)
!       call sum12734568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z290,-0.500)
!       call sum23614578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z290, 0.500)
!       call sum13624578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z290,-0.500)
!       call sum12634578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z290, 0.500)
!       call sum23514678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z290,-0.500)
!       call sum13524678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z290, 0.500)
!       call sum12534678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z290,-0.500)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z290(d,b,j,c,a,l,k,i)      ! 13724568 (+0.500) top two switched
!     & -z290(c,b,j,d,a,l,k,i)       ! 23714568 (-0.500)
!     & -z290(d,c,j,b,a,l,k,i)       ! 12734568 (-0.500)
!     & +z290(c,b,k,d,a,l,j,i)       ! 23614578 (+0.500)
!     & -z290(d,b,k,c,a,l,j,i)       ! 13624578 (-0.500)
!     & +z290(d,c,k,b,a,l,j,i)       ! 12634578 (+0.500)
!     & -z290(c,b,l,d,a,k,j,i)       ! 23514678 (-0.500)
!     & +z290(d,b,l,c,a,k,j,i)       ! 13524678 (+0.500)
!     & -z290(d,c,l,b,a,k,j,i))/2.0d0! 12534678 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z290)
       deallocate(u96)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder124356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n2,n3,n0,n2,n0,n2,t3d,f2)
       allocate(s86(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2*k2*k4
       i3=k2*k4*k4
       call egemm(i1,i2,i3,d1,f2,s86)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder4123(n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n0,n2,n0,n2,s86,d1)
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
!       allocate(z178(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k1*k2*k3*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,d1,f2,z178)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + (s86(b,l,k,m)*t3c(d,c,a,m,j,i)         !blkmdcamji      (+0.500)
     &     - s86(c,l,k,m)*t3c(d,b,a,m,j,i)          !clkmdbamji      (-0.500)
     &     + s86(d,l,k,m)*t3c(c,b,a,m,j,i)          !dlkmcbamji      (+0.500)
     &     - s86(b,l,j,m)*t3c(d,c,a,m,k,i)          !bljmdcamki      (-0.500)
     &     + s86(c,l,j,m)*t3c(d,b,a,m,k,i)          !cljmdbamki      (+0.500)
     &     - s86(d,l,j,m)*t3c(c,b,a,m,k,i)          !dljmcbamki      (-0.500)
     &     + s86(b,k,j,m)*t3c(d,c,a,m,l,i)          !bkjmdcamli      (+0.500)
     &     - s86(c,k,j,m)*t3c(d,b,a,m,l,i)          !ckjmdbamli      (-0.500)
     &     + s86(d,k,j,m)*t3c(c,b,a,m,l,i))/2.0d0   !dkjmcbamli      (+0.500)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12478356(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z178, 0.500)
!       call sum13478256(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z178,-0.500)
!       call sum23478156(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z178, 0.500)
!       call sum12468357(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z178,-0.500)
!       call sum13468257(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z178, 0.500)
!       call sum23468157(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z178,-0.500)
!       call sum12458367(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z178, 0.500)
!       call sum13458267(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z178,-0.500)
!       call sum23458167(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z178, 0.500)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z178(d,c,a,j,i,b,l,k)      ! 12478356 (+0.500)
!     & -z178(d,b,a,j,i,c,l,k)       ! 13478256 (-0.500)
!     & +z178(c,b,a,j,i,d,l,k)       ! 23478156 (+0.500)
!     & -z178(d,c,a,k,i,b,l,j)       ! 12468357 (-0.500)
!     & +z178(d,b,a,k,i,c,l,j)       ! 13468257 (+0.500)
!     & -z178(c,b,a,k,i,d,l,j)       ! 23468157 (-0.500)
!     & +z178(d,c,a,l,i,b,k,j)       ! 12458367 (+0.500)
!     & -z178(d,b,a,l,i,c,k,j)       ! 13458267 (-0.500)
!     & +z178(c,b,a,l,i,d,k,j))/2.0d0! 23458167 (+0.500)
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
       deallocate(s86)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder142356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
       allocate(u53(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k2*k3*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u53)
       deallocate(d1)
       deallocate(f2)
c
      call sum345621(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x36,u53, 1.000)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder412356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
!       allocate(z105(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k4
!       i2=k2*k2*k4*k4
!       i3=k4*k2
!       call egemm(i1,i2,i3,x36,f2,z105)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x36(m,f,d,a,j,i)*t3d(f,c,b,m,l,k)      !mfdajifcbmlk    (+1.000)
     &     - x36(m,f,c,a,j,i)*t3d(f,d,b,m,l,k)      !mfcajifdbmlk    (-1.000)
     &     + x36(m,f,b,a,j,i)*t3d(f,d,c,m,l,k)      !mfbajifdcmlk    (+1.000)
     &     - x36(m,f,d,a,k,i)*t3d(f,c,b,m,l,j)      !mfdakifcbmlj    (-1.000)
     &     + x36(m,f,c,a,k,i)*t3d(f,d,b,m,l,j)      !mfcakifdbmlj    (+1.000)
     &     - x36(m,f,b,a,k,i)*t3d(f,d,c,m,l,j)      !mfbakifdcmlj    (-1.000)
     &     + x36(m,f,d,a,l,i)*t3d(f,c,b,m,k,j)      !mfdalifcbmkj    (+1.000)
     &     - x36(m,f,c,a,l,i)*t3d(f,d,b,m,k,j)      !mfcalifdbmkj    (-1.000)
     &     + x36(m,f,b,a,l,i)*t3d(f,d,c,m,k,j)      !mfbalifdcmkj    (+1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23561478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z105, 1.000)
!       call sum13562478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z105,-1.000)
!       call sum12563478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z105, 1.000)
!       call sum23571468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z105,-1.000)
!       call sum13572468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z105, 1.000)
!       call sum12573468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z105,-1.000)
!       call sum23671458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z105, 1.000)
!       call sum13672458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z105,-1.000)
!       call sum12673458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z105, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z105(c,b,l,k,d,a,j,i)       ! 23561478 (+1.000)
!     & -z105(d,b,l,k,c,a,j,i)       ! 13562478 (-1.000)
!     & +z105(d,c,l,k,b,a,j,i)       ! 12563478 (+1.000)
!     & -z105(c,b,l,j,d,a,k,i)       ! 23571468 (-1.000)
!     & +z105(d,b,l,j,c,a,k,i)       ! 13572468 (+1.000)
!     & -z105(d,c,l,j,b,a,k,i)       ! 12573468 (-1.000)
!     & +z105(c,b,k,j,d,a,l,i)       ! 23671458 (+1.000)
!     & -z105(d,b,k,j,c,a,l,i)       ! 13672458 (-1.000)
!     & +z105(d,c,k,j,b,a,l,i)       ! 12673458 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z105)
       deallocate(x36)
c
       allocate(f1(n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder561234(n2,n3,n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,u53,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u94(n0+1:n2,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u94)
       deallocate(f1)
       deallocate(b2)
       deallocate(u53)
c
      call sum512346(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x47,u94, 1.000)
       deallocate(u94)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z146(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x47,d2,z146)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + x47(m,b,a,l,j,i)*t2c(d,c,m,k)          !mbaljidcmk      (+1.000)
     &     - x47(m,c,a,l,j,i)*t2c(d,b,m,k)          !mcaljidbmk      (-1.000)
     &     + x47(m,d,a,l,j,i)*t2c(c,b,m,k)          !mdaljicbmk      (+1.000)
     &     - x47(m,b,a,k,j,i)*t2c(d,c,m,l)          !mbakjidcml      (-1.000)
     &     + x47(m,c,a,k,j,i)*t2c(d,b,m,l)          !mcakjidbml      (+1.000)
     &     - x47(m,d,a,k,j,i)*t2c(c,b,m,l)          !mdakjicbml      (-1.000)
     &     - x47(m,b,a,l,k,i)*t2c(d,c,m,j)          !mbalkidcmj      (-1.000)
     &     + x47(m,c,a,l,k,i)*t2c(d,b,m,j)          !mcalkidbmj      (+1.000)
     &     - x47(m,d,a,l,k,i)*t2c(c,b,m,j)          !mdalkicbmj      (-1.000)
     &     + x47(m,b,a,k,l,i)*t2c(d,c,m,j)          !mbaklidcmj      (+1.000)
     &     - x47(m,c,a,k,l,i)*t2c(d,b,m,j)          !mcaklidbmj      (-1.000)
     &     + x47(m,d,a,k,l,i)*t2c(c,b,m,j)          !mdaklicbmj      (+1.000)
     &     + x47(m,b,a,j,k,i)*t2c(d,c,m,l)          !mbajkidcml      (+1.000)
     &     - x47(m,c,a,j,k,i)*t2c(d,b,m,l)          !mcajkidbml      (-1.000)
     &     + x47(m,d,a,j,k,i)*t2c(c,b,m,l)          !mdajkicbml      (+1.000)
     &     - x47(m,b,a,j,l,i)*t2c(d,c,m,k)          !mbajlidcmk      (-1.000)
     &     + x47(m,c,a,j,l,i)*t2c(d,b,m,k)          !mcajlidbmk      (+1.000)
     &     - x47(m,d,a,j,l,i)*t2c(c,b,m,k)          !mdajlicbmk      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12634578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146, 1.000)
!       call sum13624578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146,-1.000)
!       call sum23614578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146, 1.000)
!       call sum12534678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146,-1.000)
!       call sum13524678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146, 1.000)
!       call sum23514678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146,-1.000)
!       call sum12734568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146,-1.000)
!       call sum13724568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146, 1.000)
!       call sum23714568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146,-1.000)
!       call sum12734658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146, 1.000)
!       call sum13724658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146,-1.000)
!       call sum23714658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146, 1.000)
!       call sum12534768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146, 1.000)
!       call sum13524768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146,-1.000)
!       call sum23514768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146, 1.000)
!       call sum12634758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146,-1.000)
!       call sum13624758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146, 1.000)
!       call sum23614758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z146,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z146(d,c,k,b,a,l,j,i)       ! 12634578 (+1.000)
!     & -z146(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!     & +z146(c,b,k,d,a,l,j,i)       ! 23614578 (+1.000)
!     & -z146(d,c,l,b,a,k,j,i)       ! 12534678 (-1.000)
!     & +z146(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & -z146(c,b,l,d,a,k,j,i)       ! 23514678 (-1.000)
!     & -z146(d,c,j,b,a,l,k,i)       ! 12734568 (-1.000)
!     & +z146(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!     & -z146(c,b,j,d,a,l,k,i)       ! 23714568 (-1.000)
!     & +z146(d,c,j,b,a,k,l,i)       ! 12734658 (+1.000)
!     & -z146(d,b,j,c,a,k,l,i)       ! 13724658 (-1.000)
!     & +z146(c,b,j,d,a,k,l,i)       ! 23714658 (+1.000)
!     & +z146(d,c,l,b,a,j,k,i)       ! 12534768 (+1.000)
!     & -z146(d,b,l,c,a,j,k,i)       ! 13524768 (-1.000)
!     & +z146(c,b,l,d,a,j,k,i)       ! 23514768 (+1.000)
!     & -z146(d,c,k,b,a,j,l,i)       ! 12634758 (-1.000)
!     & +z146(d,b,k,c,a,j,l,i)       ! 13624758 (+1.000)
!     & -z146(c,b,k,d,a,j,l,i)       ! 23614758 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z146)
       deallocate(x47)
c
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder124356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n2,n3,n0,n2,n1,n3,n0,n2,n0,n1,t3c,f2)
       allocate(s87(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k2*k3
       i3=k2*k4*k4
       call egemm(i1,i2,i3,d1,f2,s87)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x5,s87,-0.500)
       deallocate(s87)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder123456(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,t3d,f2)
       allocate(u54(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k2*k2*k4
       i3=k4*k4
       call egemm(i1,i2,i3,d1,f2,u54)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
!       call reorder651234(n2,n3,n0,n2,n0,n2,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,n0,n2,u54,f1)
!       allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder451236(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n0,n2,n2,n3,n2,n3,n1,n3,n0,n1,t3c,f2)
!       allocate(z181(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k2*k4
!       i2=k1*k3*k4*k4
!       i3=k2*k2
!       call egemm(i1,i2,i3,f1,f2,z181)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2;do n=n0+1,n2
             sum=sum
     &     + (u54(b,l,k,j,m,n)*t3c(d,c,a,n,m,i)       !blkjmndcanmi    (+0.250)
     &     - u54(c,l,k,j,m,n)*t3c(d,b,a,n,m,i)        !clkjmndbanmi    (-0.250)
     &     + u54(d,l,k,j,m,n)*t3c(c,b,a,n,m,i))/4.0d0 !dlkjmncbanmi    (+0.250)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12483567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z181, 0.250)
!       call sum13482567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z181,-0.250)
!       call sum23481567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z181, 0.250)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z181(d,c,a,i,b,l,k,j)      ! 12483567 (+0.250)
!     & -z181(d,b,a,i,c,l,k,j)       ! 13482567 (-0.250)
!     & +z181(c,b,a,i,d,l,k,j))/4.0d0! 23481567 (+0.250)
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
c
       allocate(f1(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder561234(n2,n3,n0,n2,n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,n0,n2,u54,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u90(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2*k4*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u90)
       deallocate(f1)
       deallocate(b2)
       deallocate(u54)
c
      call sum213456(n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,x38,u90,-1.000)
       deallocate(u90)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z112(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k2*k4*k4
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x38,d2,z112)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + (x38(m,d,b,l,k,j)*t2b(c,a,m,i)         !mdblkjcami      (+0.500)
     &     - x38(m,d,c,l,k,j)*t2b(b,a,m,i)          !mdclkjbami      (-0.500)
     &     - x38(m,c,b,l,k,j)*t2b(d,a,m,i)          !mcblkjdami      (-0.500)
     &     + x38(m,b,c,l,k,j)*t2b(d,a,m,i)          !mbclkjdami      (+0.500)
     &     + x38(m,c,d,l,k,j)*t2b(b,a,m,i)          !mcdlkjbami      (+0.500)
     &     - x38(m,b,d,l,k,j)*t2b(c,a,m,i))/2.0d0   !mbdlkjcami      (-0.500)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum24813567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z112, 0.500)
!       call sum34812567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z112,-0.500)
!       call sum14823567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z112,-0.500)
!       call sum14832567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z112, 0.500)
!       call sum34821567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z112, 0.500)
!       call sum24831567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z112,-0.500)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z112(c,a,i,d,b,l,k,j)      ! 24813567 (+0.500)
!     & -z112(b,a,i,d,c,l,k,j)       ! 34812567 (-0.500)
!     & -z112(d,a,i,c,b,l,k,j)       ! 14823567 (-0.500)
!     & +z112(d,a,i,b,c,l,k,j)       ! 14832567 (+0.500)
!     & +z112(b,a,i,c,d,l,k,j)       ! 34821567 (+0.500)
!     & -z112(c,a,i,b,d,l,k,j))/2.0d0! 24831567 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z112)
       deallocate(x38)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n2,n3,intb,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder154236(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n0,n2,n0,n2,n2,n3,n1,n3,n0,n1,t3c,f2)
       allocate(s88(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k3*k4
       i3=k2*k2*k4
       call egemm(i1,i2,i3,d1,f2,s88)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n2,n3,n2,n3,n1,n3,n0,n1,x6,s88, 0.500)
       deallocate(s88)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s91(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s91)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n0,n1,s91,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder361245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
       allocate(u59(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k2*k4*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u59)
       deallocate(d1)
       deallocate(f2)
c
      call sum234516(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x30,u59, 1.000)
       deallocate(u59)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3214(n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n0,n1,n0,n1,s91,d1)
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
     & n0,n1,n1,n3,n0,n1,n0,n1,s91,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s92(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s92)
       deallocate(d1)
       deallocate(b2)
       deallocate(s91)
c
       call sum3241(n0,n1,n1,n3,n1,n3,n0,n1,x12,s92,-1.000)
       deallocate(s92)
c
       call sumx3142(n0,n3,n0,n1,n1,n3,n1,n3,n0,n1,x12,intr, 1.000)
c
!       allocate(h2(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n2))
!       call reorder84123567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n1,n1,n3,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,t4d,h2)
!       allocate(z12(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3
!       i2=k2*k2*k2*k4*k4*k4
!       i3=k3*k1
!       call egemm(i1,i2,i3,x12,h2,z12)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + x12(m,e,a,i)*t4d(d,c,b,e,l,k,j,m)      !meaidcbelkjm    (+1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12356748(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z12, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z12(d,c,b,l,k,j,a,i)       ! 12356748 (+1.000)
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
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(q22(n1+1:n3,n0+1:n1))
       i1=k1*k3
       i3=k1*k3
       call egemm1(i1,i3,d1,b2,q22)
       deallocate(d1)
       deallocate(b2)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,q22,b1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(s112(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
       i1=k3
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s112)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x2,s112, 1.000)
       deallocate(s112)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,q22,b1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q23(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,b1,b2,q23)
       deallocate(b1)
       deallocate(b2)
       deallocate(q22)
c
       call sum21(n1,n3,n1,n3,x9,q23, 1.000)
       deallocate(q23)
c
       allocate(d1(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s93(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s93)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n0,n1,s93,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder142356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
       allocate(u61(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k2*k4*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u61)
       deallocate(d1)
       deallocate(f2)
c
      call sum234516(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x30,u61, 1.000)
       deallocate(u61)
c
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z84(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k4*k4
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x30,d2,z84)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - x30(m,c,b,l,k,i)*t2b(d,a,j,m)          !mcblkidajm      (-1.000)
     &     + x30(m,d,b,l,k,i)*t2b(c,a,j,m)          !mdblkicajm      (+1.000)
     &     - x30(m,d,c,l,k,i)*t2b(b,a,j,m)          !mdclkibajm      (-1.000)
     &     + x30(m,c,b,l,j,i)*t2b(d,a,k,m)          !mcbljidakm      (+1.000)
     &     - x30(m,d,b,l,j,i)*t2b(c,a,k,m)          !mdbljicakm      (-1.000)
     &     + x30(m,d,c,l,j,i)*t2b(b,a,k,m)          !mdcljibakm      (+1.000)
     &     - x30(m,c,b,k,j,i)*t2b(d,a,l,m)          !mcbkjidalm      (-1.000)
     &     + x30(m,d,b,k,j,i)*t2b(c,a,l,m)          !mdbkjicalm      (+1.000)
     &     - x30(m,d,c,k,j,i)*t2b(b,a,l,m)          !mdckjibalm      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14723568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z84,-1.000)
!       call sum24713568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z84, 1.000)
!       call sum34712568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z84,-1.000)
!       call sum14623578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z84, 1.000)
!       call sum24613578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z84,-1.000)
!       call sum34612578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z84, 1.000)
!       call sum14523678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z84,-1.000)
!       call sum24513678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z84, 1.000)
!       call sum34512678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z84,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z84(d,a,j,c,b,l,k,i)       ! 14723568 (-1.000)
!     & +z84(c,a,j,d,b,l,k,i)       ! 24713568 (+1.000)
!     & -z84(b,a,j,d,c,l,k,i)       ! 34712568 (-1.000)
!     & +z84(d,a,k,c,b,l,j,i)       ! 14623578 (+1.000)
!     & -z84(c,a,k,d,b,l,j,i)       ! 24613578 (-1.000)
!     & +z84(b,a,k,d,c,l,j,i)       ! 34612578 (+1.000)
!     & -z84(d,a,l,c,b,k,j,i)       ! 14523678 (-1.000)
!     & +z84(c,a,l,d,b,k,j,i)       ! 24513678 (+1.000)
!     & -z84(b,a,l,d,c,k,j,i)       ! 34512678 (-1.000)
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
       deallocate(x30)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n0,n1,s93,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q24(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q24)
       deallocate(d1)
       deallocate(b2)
c
       x8=x8+q24
       deallocate(q24)
c
       call sumx12(0,n3,n0,n1,n0,n1,x8,fockr, 1.000)
c
!       allocate(h2(n0+1:n1,n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n2))
!       call reorder81234567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n1,n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,t4d,h2)
!       allocate(z8(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1
!       i2=k2*k2*k2*k3*k4*k4*k4
!       i3=k1
!       call egemm(i1,i2,i3,x8,h2,z8)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - x8(m,i)*t4d(d,c,b,a,l,k,j,m)           !midcbalkjm      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       v4d=v4d-z8
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z8(d,c,b,a,l,k,j,i)       ! 12345678 (-1.000)
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
     & n0,n2,n0,n1,n2,n3,n0,n1,s93,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s104(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s104)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n2,n3,n2,n3,n0,n1,x14,s104,-1.000)
       deallocate(s104)
c
       call sumx1342(n0,n3,n0,n1,n2,n3,n2,n3,n0,n1,x14,intm, 1.000)
c
!       allocate(h2(n0+1:n1,n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n2))
!       call reorder81234567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n1,n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,t4d,h2)
!       allocate(z14(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,
!     & n2+1:n3,n0+1:n1))
!       i1=k1*k4
!       i2=k2*k2*k2*k3*k4*k4
!       i3=k4*k1
!       call egemm(i1,i2,i3,x14,h2,z14)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n1
             sum=sum
     &     - x14(m,e,d,i)*t4d(e,c,b,a,l,k,j,m)      !mediecbalkjm    (-1.000)
     &     + x14(m,e,c,i)*t4d(e,d,b,a,l,k,j,m)      !meciedbalkjm    (+1.000)
     &     - x14(m,e,b,i)*t4d(e,d,c,a,l,k,j,m)      !mebiedcalkjm    (-1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23456718(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z14,-1.000)
!       call sum13456728(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z14, 1.000)
!       call sum12456738(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z14,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z14(c,b,a,l,k,j,d,i)       ! 23456718 (-1.000)
!     & +z14(d,b,a,l,k,j,c,i)       ! 13456728 (+1.000)
!     & -z14(d,c,a,l,k,j,b,i)       ! 12456738 (-1.000)
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
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder2314(n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n1,n0,n1,n0,n2,s93,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s103(n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s103)
       deallocate(d1)
       deallocate(b2)
c
       call sum3241(n0,n2,n0,n1,n0,n2,n0,n1,x13,s103, 1.000)
c
       call sumx2143(n0,n3,n0,n2,n0,n1,n0,n2,n0,n1,x13,intm, 1.000)
c
!       allocate(h2(n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2))
!       call reorder58123467(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n2,n0,n1,n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,t4d,h2)
!       allocate(z13(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2
!       i2=k2*k2*k3*k4*k4*k4
!       i3=k1*k2
!       call egemm(i1,i2,i3,x13,h2,z13)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     + x13(n,m,j,i)*t4d(d,c,b,a,n,l,k,m)      !nmjidcbanlkm    (+1.000)
     &     - x13(n,m,k,i)*t4d(d,c,b,a,n,l,j,m)      !nmkidcbanljm    (-1.000)
     &     + x13(n,m,l,i)*t4d(d,c,b,a,n,k,j,m)      !nmlidcbankjm    (+1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       v4d=v4d+z13
!       call sum12345768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z13,-1.000)
!       call sum12346758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z13, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z13(d,c,b,a,l,k,j,i)       ! 12345678 (+1.000)
!     & -z13(d,c,b,a,l,j,k,i)       ! 12345768 (-1.000)
!     & +z13(d,c,b,a,k,j,l,i)       ! 12346758 (+1.000)
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
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder2413(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s103,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u113(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u113)
       deallocate(d1)
       deallocate(d2)
c
      call sum234156(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x21,u113,1.000)
       deallocate(u113)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z205(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x21,d2,z205)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     + x21(n,d,a,k,j,i)*t2c(c,b,n,l)          !ndakjicbnl      (+1.000)
     &     - x21(n,c,a,k,j,i)*t2c(d,b,n,l)          !ncakjidbnl      (-1.000)
     &     + x21(n,b,a,k,j,i)*t2c(d,c,n,l)          !nbakjidcnl      (+1.000)
     &     - x21(n,d,a,l,j,i)*t2c(c,b,n,k)          !ndaljicbnk      (-1.000)
     &     + x21(n,c,a,l,j,i)*t2c(d,b,n,k)          !ncaljidbnk      (+1.000)
     &     - x21(n,b,a,l,j,i)*t2c(d,c,n,k)          !nbaljidcnk      (-1.000)
     &     - x21(n,d,a,j,k,i)*t2c(c,b,n,l)          !ndajkicbnl      (-1.000)
     &     + x21(n,c,a,j,k,i)*t2c(d,b,n,l)          !ncajkidbnl      (+1.000)
     &     - x21(n,b,a,j,k,i)*t2c(d,c,n,l)          !nbajkidcnl      (-1.000)
     &     + x21(n,d,a,j,l,i)*t2c(c,b,n,k)          !ndajlicbnk      (+1.000)
     &     - x21(n,c,a,j,l,i)*t2c(d,b,n,k)          !ncajlidbnk      (-1.000)
     &     + x21(n,b,a,j,l,i)*t2c(d,c,n,k)          !nbajlidcnk      (+1.000)
     &     + x21(n,d,a,l,k,i)*t2c(c,b,n,j)          !ndalkicbnj      (+1.000)
     &     - x21(n,c,a,l,k,i)*t2c(d,b,n,j)          !ncalkidbnj      (-1.000)
     &     + x21(n,b,a,l,k,i)*t2c(d,c,n,j)          !nbalkidcnj      (+1.000)
     &     - x21(n,d,a,k,l,i)*t2c(c,b,n,j)          !ndaklicbnj      (-1.000)
     &     + x21(n,c,a,k,l,i)*t2c(d,b,n,j)          !ncaklidbnj      (+1.000)
     &     - x21(n,b,a,k,l,i)*t2c(d,c,n,j)          !nbaklidcnj      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23514678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205, 1.000)
!       call sum13524678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205,-1.000)
!       call sum12534678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205, 1.000)
!       call sum23614578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205,-1.000)
!       call sum13624578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205, 1.000)
!       call sum12634578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205,-1.000)
!       call sum23514768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205,-1.000)
!       call sum13524768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205, 1.000)
!       call sum12534768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205,-1.000)
!       call sum23614758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205, 1.000)
!       call sum13624758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205,-1.000)
!       call sum12634758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205, 1.000)
!       call sum23714568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205, 1.000)
!       call sum13724568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205,-1.000)
!       call sum12734568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205, 1.000)
!       call sum23714658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205,-1.000)
!       call sum13724658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205, 1.000)
!       call sum12734658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z205,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z205(c,b,l,d,a,k,j,i)       ! 23514678 (+1.000)
!     & -z205(d,b,l,c,a,k,j,i)       ! 13524678 (-1.000)
!     & +z205(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & -z205(c,b,k,d,a,l,j,i)       ! 23614578 (-1.000)
!     & +z205(d,b,k,c,a,l,j,i)       ! 13624578 (+1.000)
!     & -z205(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & -z205(c,b,l,d,a,j,k,i)       ! 23514768 (-1.000)
!     & +z205(d,b,l,c,a,j,k,i)       ! 13524768 (+1.000)
!     & -z205(d,c,l,b,a,j,k,i)       ! 12534768 (-1.000)
!     & +z205(c,b,k,d,a,j,l,i)       ! 23614758 (+1.000)
!     & -z205(d,b,k,c,a,j,l,i)       ! 13624758 (-1.000)
!     & +z205(d,c,k,b,a,j,l,i)       ! 12634758 (+1.000)
!     & +z205(c,b,j,d,a,l,k,i)       ! 23714568 (+1.000)
!     & -z205(d,b,j,c,a,l,k,i)       ! 13724568 (-1.000)
!     & +z205(d,c,j,b,a,l,k,i)       ! 12734568 (+1.000)
!     & -z205(c,b,j,d,a,k,l,i)       ! 23714658 (-1.000)
!     & +z205(d,b,j,c,a,k,l,i)       ! 13724658 (+1.000)
!     & -z205(d,c,j,b,a,k,l,i)       ! 12734658 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z205)
       deallocate(x21)
c
       allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       call reorder4213(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n2,n0,n1,n0,n2,n0,n1,s103,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s157(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s157)
       deallocate(d1)
       deallocate(b2)
       deallocate(s103)
c
       call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s157,-1.000)
       deallocate(s157)
c
       call sumx1243(n0,n3,n0,n1,n2,n3,n0,n2,n0,n1,x1,intm, 1.000)
c
!       allocate(f2(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
!       call reorder612345(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,t3c,f2)
!       allocate(z1(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k4
!       i2=k2*k2*k3*k4*k4
!       i3=k1
!       call egemm(i1,i2,i3,x1,f2,z1)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - x1(m,d,j,i)*t3c(c,b,a,l,k,m)           !mdjicbalkm      (-1.000)
     &     + x1(m,c,j,i)*t3c(d,b,a,l,k,m)           !mcjidbalkm      (+1.000)
     &     - x1(m,b,j,i)*t3c(d,c,a,l,k,m)           !mbjidcalkm      (-1.000)
     &     + x1(m,d,k,i)*t3c(c,b,a,l,j,m)           !mdkicbaljm      (+1.000)
     &     - x1(m,c,k,i)*t3c(d,b,a,l,j,m)           !mckidbaljm      (-1.000)
     &     + x1(m,b,k,i)*t3c(d,c,a,l,j,m)           !mbkidcaljm      (+1.000)
     &     - x1(m,d,l,i)*t3c(c,b,a,k,j,m)           !mdlicbakjm      (-1.000)
     &     + x1(m,c,l,i)*t3c(d,b,a,k,j,m)           !mclidbakjm      (+1.000)
     &     - x1(m,b,l,i)*t3c(d,c,a,k,j,m)           !mblidcakjm      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23456178(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z1,-1.000)
!       call sum13456278(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z1, 1.000)
!       call sum12456378(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z1,-1.000)
!       call sum23457168(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z1, 1.000)
!       call sum13457268(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z1,-1.000)
!       call sum12457368(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z1, 1.000)
!       call sum23467158(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z1,-1.000)
!       call sum13467258(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z1, 1.000)
!       call sum12467358(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z1,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z1(c,b,a,l,k,d,j,i)       ! 23456178 (-1.000)
!     & +z1(d,b,a,l,k,c,j,i)       ! 13456278 (+1.000)
!     & -z1(d,c,a,l,k,b,j,i)       ! 12456378 (-1.000)
!     & +z1(c,b,a,l,j,d,k,i)       ! 23457168 (+1.000)
!     & -z1(d,b,a,l,j,c,k,i)       ! 13457268 (-1.000)
!     & +z1(d,c,a,l,j,b,k,i)       ! 12457368 (+1.000)
!     & -z1(c,b,a,k,j,d,l,i)       ! 23467158 (-1.000)
!     & +z1(d,b,a,k,j,c,l,i)       ! 13467258 (+1.000)
!     & -z1(d,c,a,k,j,b,l,i)       ! 12467358 (-1.000)
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
     & n0,n1,n2,n3,n0,n1,n0,n2,s93,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s94(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s94)
       deallocate(d1)
       deallocate(b2)
       deallocate(s93)
c
       call sum3241(n0,n2,n2,n3,n1,n3,n0,n1,x20,s94,-1.000)
c
       call sumx3142(n0,n3,n0,n2,n2,n3,n1,n3,n0,n1,x20,intm, 1.000)
c
!       allocate(h2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n2))
!       call reorder51234678(n2,n3,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,t4e,h2)
!       allocate(z20(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3
!       i2=k2*k2*k2*k4*k4*k4
!       i3=k4*k2
!       call egemm(i1,i2,i3,x20,h2,z20)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x20(m,e,a,i)*t4e(e,d,c,b,m,l,k,j)      !meaiedcbmlkj    (+1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12356748(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z20, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z20(d,c,b,l,k,j,a,i)       ! 12356748 (+1.000)
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
     & n0,n2,n2,n3,n1,n3,n0,n1,s94,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s156(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s156)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s156, 1.000)
       deallocate(s156)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2413(n1,n3,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s94,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s155(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s155)
       deallocate(d1)
       deallocate(b2)
       deallocate(s94)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s155,-1.000)
       deallocate(s155)
c
       call sumx2143(n0,n3,n0,n2,n1,n3,n0,n2,n0,n1,x5,intm, 1.000)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder412356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
!       allocate(z5(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3
!       i2=k2*k2*k4*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x5,f2,z5)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     - x5(m,a,j,i)*t3d(d,c,b,m,l,k)           !majidcbmlk      (-1.000)
     &     + x5(m,a,k,i)*t3d(d,c,b,m,l,j)           !makidcbmlj      (+1.000)
     &     - x5(m,a,l,i)*t3d(d,c,b,m,k,j)           !malidcbmkj      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12356478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z5,-1.000)
!       call sum12357468(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z5, 1.000)
!       call sum12367458(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z5,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z5(d,c,b,l,k,a,j,i)       ! 12356478 (-1.000)
!     & +z5(d,c,b,l,j,a,k,i)       ! 12357468 (+1.000)
!     & -z5(d,c,b,k,j,a,l,i)       ! 12367458 (-1.000)
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
       allocate(s105(n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s105)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder2431(n0,n2,n1,n3,n0,n1,n0,n2,
     & n1,n3,n0,n2,n0,n1,n0,n2,s105,d1)
       allocate(f2(n1+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder341256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n1,n3,n0,n2,n2,n3,n2,n3,n0,n2,n0,n1,t3c,f2)
       allocate(u85(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k2*k4*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,f2,u85)
       deallocate(d1)
       deallocate(f2)
c
      call sum234615(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x35,u85, 1.000)
       deallocate(u85)
c
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z94(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k4*k4
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x35,d2,z94)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x35(m,c,b,l,j,i)*t2b(d,a,k,m)          !mcbljidakm      (+1.000)
     &     - x35(m,d,b,l,j,i)*t2b(c,a,k,m)          !mdbljicakm      (-1.000)
     &     + x35(m,d,c,l,j,i)*t2b(b,a,k,m)          !mdcljibakm      (+1.000)
     &     - x35(m,c,b,k,j,i)*t2b(d,a,l,m)          !mcbkjidalm      (-1.000)
     &     + x35(m,d,b,k,j,i)*t2b(c,a,l,m)          !mdbkjicalm      (+1.000)
     &     - x35(m,d,c,k,j,i)*t2b(b,a,l,m)          !mdckjibalm      (-1.000)
     &     - x35(m,c,b,l,k,i)*t2b(d,a,j,m)          !mcblkidajm      (-1.000)
     &     + x35(m,d,b,l,k,i)*t2b(c,a,j,m)          !mdblkicajm      (+1.000)
     &     - x35(m,d,c,l,k,i)*t2b(b,a,j,m)          !mdclkibajm      (-1.000)
     &     + x35(m,c,b,k,l,i)*t2b(d,a,j,m)          !mcbklidajm      (+1.000)
     &     - x35(m,d,b,k,l,i)*t2b(c,a,j,m)          !mdbklicajm      (-1.000)
     &     + x35(m,d,c,k,l,i)*t2b(b,a,j,m)          !mdcklibajm      (+1.000)
     &     + x35(m,c,b,j,k,i)*t2b(d,a,l,m)          !mcbjkidalm      (+1.000)
     &     - x35(m,d,b,j,k,i)*t2b(c,a,l,m)          !mdbjkicalm      (-1.000)
     &     + x35(m,d,c,j,k,i)*t2b(b,a,l,m)          !mdcjkibalm      (+1.000)
     &     - x35(m,c,b,j,l,i)*t2b(d,a,k,m)          !mcbjlidakm      (-1.000)
     &     + x35(m,d,b,j,l,i)*t2b(c,a,k,m)          !mdbjlicakm      (+1.000)
     &     - x35(m,d,c,j,l,i)*t2b(b,a,k,m)          !mdcjlibakm      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14623578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94, 1.000)
!       call sum24613578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94,-1.000)
!       call sum34612578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94, 1.000)
!       call sum14523678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94,-1.000)
!       call sum24513678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94, 1.000)
!       call sum34512678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94,-1.000)
!       call sum14723568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94,-1.000)
!       call sum24713568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94, 1.000)
!       call sum34712568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94,-1.000)
!       call sum14723658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94, 1.000)
!       call sum24713658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94,-1.000)
!       call sum34712658(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94, 1.000)
!       call sum14523768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94, 1.000)
!       call sum24513768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94,-1.000)
!       call sum34512768(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94, 1.000)
!       call sum14623758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94,-1.000)
!       call sum24613758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94, 1.000)
!       call sum34612758(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z94,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z94(d,a,k,c,b,l,j,i)       ! 14623578 (+1.000)
!     & -z94(c,a,k,d,b,l,j,i)       ! 24613578 (-1.000)
!     & +z94(b,a,k,d,c,l,j,i)       ! 34612578 (+1.000)
!     & -z94(d,a,l,c,b,k,j,i)       ! 14523678 (-1.000)
!     & +z94(c,a,l,d,b,k,j,i)       ! 24513678 (+1.000)
!     & -z94(b,a,l,d,c,k,j,i)       ! 34512678 (-1.000)
!     & -z94(d,a,j,c,b,l,k,i)       ! 14723568 (-1.000)
!     & +z94(c,a,j,d,b,l,k,i)       ! 24713568 (+1.000)
!     & -z94(b,a,j,d,c,l,k,i)       ! 34712568 (-1.000)
!     & +z94(d,a,j,c,b,k,l,i)       ! 14723658 (+1.000)
!     & -z94(c,a,j,d,b,k,l,i)       ! 24713658 (-1.000)
!     & +z94(b,a,j,d,c,k,l,i)       ! 34712658 (+1.000)
!     & +z94(d,a,l,c,b,j,k,i)       ! 14523768 (+1.000)
!     & -z94(c,a,l,d,b,j,k,i)       ! 24513768 (-1.000)
!     & +z94(b,a,l,d,c,j,k,i)       ! 34512768 (+1.000)
!     & -z94(d,a,k,c,b,j,l,i)       ! 14623758 (-1.000)
!     & +z94(c,a,k,d,b,j,l,i)       ! 24613758 (+1.000)
!     & -z94(b,a,k,d,c,j,l,i)       ! 34612758 (-1.000)
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
       deallocate(x35)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder3214(n0,n2,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n0,n2,n0,n2,s105,d1)
       allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder631245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
       allocate(u82(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k2*k4*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,f2,u82)
       deallocate(d1)
       deallocate(f2)
c
      call sum234561(n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,x33,u82, 1.000)
       deallocate(u82)
c
       allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder4321(n0,n2,n1,n3,n0,n1,n0,n2,
     & n0,n2,n0,n1,n1,n3,n0,n2,s105,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s129(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s129)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n1,n3,n2,n3,n0,n2,x7,s129,-1.000)
       deallocate(s129)
c
       call sumx1324(n0,n3,n0,n1,n1,n3,n2,n3,n0,n2,x7,intm, 1.000)
c
!       allocate(h2(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       call reorder73124568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t4c,h2)
!       allocate(z7(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,
!     & n2+1:n3,n0+1:n2))
!       i1=k2*k4
!       i2=k1*k2*k2*k3*k4*k4
!       i3=k3*k1
!       call egemm(i1,i2,i3,x7,h2,z7)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + x7(m,e,d,j)*t4c(c,b,e,a,l,k,m,i)       !medjcbealkmi    (+1.000)
     &     - x7(m,e,c,j)*t4c(d,b,e,a,l,k,m,i)       !mecjdbealkmi    (-1.000)
     &     + x7(m,e,b,j)*t4c(d,c,e,a,l,k,m,i)       !mebjdcealkmi    (+1.000)
     &     - x7(m,e,d,k)*t4c(c,b,e,a,l,j,m,i)       !medkcbealjmi    (-1.000)
     &     + x7(m,e,c,k)*t4c(d,b,e,a,l,j,m,i)       !meckdbealjmi    (+1.000)
     &     - x7(m,e,b,k)*t4c(d,c,e,a,l,j,m,i)       !mebkdcealjmi    (-1.000)
     &     + x7(m,e,d,l)*t4c(c,b,e,a,k,j,m,i)       !medlcbeakjmi    (+1.000)
     &     - x7(m,e,c,l)*t4c(d,b,e,a,k,j,m,i)       !mecldbeakjmi    (-1.000)
     &     + x7(m,e,b,l)*t4c(d,c,e,a,k,j,m,i)       !mebldceakjmi    (+1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23456817(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z7, 1.000)
!       call sum13456827(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z7,-1.000)
!       call sum12456837(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z7, 1.000)
!       call sum23457816(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z7,-1.000)
!       call sum13457826(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z7, 1.000)
!       call sum12457836(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z7,-1.000)
!       call sum23467815(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z7, 1.000)
!       call sum13467825(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z7,-1.000)
!       call sum12467835(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z7, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z7(c,b,a,l,k,i,d,j)       ! 23456817 (+1.000)
!     & -z7(d,b,a,l,k,i,c,j)       ! 13456827 (-1.000)
!     & +z7(d,c,a,l,k,i,b,j)       ! 12456837 (+1.000)
!     & -z7(c,b,a,l,j,i,d,k)       ! 23457816 (-1.000)
!     & +z7(d,b,a,l,j,i,c,k)       ! 13457826 (+1.000)
!     & -z7(d,c,a,l,j,i,b,k)       ! 12457836 (-1.000)
!     & +z7(c,b,a,k,j,i,d,l)       ! 23467815 (+1.000)
!     & -z7(d,b,a,k,j,i,c,l)       ! 13467825 (-1.000)
!     & +z7(d,c,a,k,j,i,b,l)       ! 12467835 (+1.000)
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
     & n0,n1,n1,n3,n0,n2,n0,n2,s105,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q25(n0+1:n2,n0+1:n2))
       i1=k2*k2
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q25)
       deallocate(d1)
       deallocate(b2)
c
       call sum21(n0,n2,n0,n2,x10,q25, 1.000)
       deallocate(q25)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder3214(n0,n2,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n0,n2,n0,n2,s105,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s106(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s106)
       deallocate(d1)
       deallocate(b2)
       deallocate(s105)
c
       call sum3241(n0,n2,n1,n3,n1,n3,n0,n2,x15,s106,-1.000)
c
       call sumx3124(n0,n3,n0,n2,n1,n3,n1,n3,n0,n2,x15,intm, 1.000)
c
!       allocate(h2(n0+1:n2,n1+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       call reorder54123678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n2,n1,n3,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,t4d,h2)
!       allocate(z15(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,
!     & n1+1:n3,n0+1:n2))
!       i1=k2*k3
!       i2=k1*k2*k2*k4*k4*k4
!       i3=k3*k2
!       call egemm(i1,i2,i3,x15,h2,z15)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n2
             sum=sum
     &     - x15(m,e,a,j)*t4d(d,c,b,e,m,l,k,i)      !meajdcbemlki    (-1.000)
     &     + x15(m,e,a,k)*t4d(d,c,b,e,m,l,j,i)      !meakdcbemlji    (+1.000)
     &     - x15(m,e,a,l)*t4d(d,c,b,e,m,k,j,i)      !mealdcbemkji    (-1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12356847(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z15,-1.000)
!       call sum12357846(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z15, 1.000)
!       call sum12367845(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z15,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z15(d,c,b,l,k,i,a,j)       ! 12356847 (-1.000)
!     & +z15(d,c,b,l,j,i,a,k)       ! 12357846 (+1.000)
!     & -z15(d,c,b,k,j,i,a,l)       ! 12367845 (-1.000)
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
c
       allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4213(n1,n3,n1,n3,n0,n2,n0,n2,
     & n0,n2,n1,n3,n1,n3,n0,n2,s106,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s158(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s158)
       deallocate(d1)
       deallocate(b2)
       deallocate(s106)
c
       call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s158, 1.000)
       deallocate(s158)
c
       allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s107(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s107)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       call reorder2431(n1,n3,n2,n3,n1,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n1,n3,s107,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q28(n1+1:n3,n1+1:n3))
       i1=k3*k3
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q28)
       deallocate(d1)
       deallocate(b2)
c
       x9=x9-q28
       deallocate(q28)
c
       call sumx21(0,n3,n1,n3,n1,n3,x9,fockr, 1.000)
c
!       allocate(h2(n1+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       call reorder41235678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n1,n3,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,n0,n1,t4d,h2)
!       allocate(z9(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,
!     & n0+1:n1,n1+1:n3))
!       i1=k3
!       i2=k1*k2*k2*k2*k4*k4*k4
!       i3=k3
!       call egemm(i1,i2,i3,x9,h2,z9)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + x9(e,a)*t4d(d,c,b,e,l,k,j,i)           !eadcbelkji      (+1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12356784(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z9, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
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
     & n0,n2,n2,n3,n1,n3,n1,n3,s107,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s108(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s108)
       deallocate(d1)
       deallocate(b2)
       deallocate(s107)
c
       call sum3124(n2,n3,n1,n3,n2,n3,n1,n3,x16,s108, 1.000)
       deallocate(s108)
c
       call sumx4321(n0,n3,n2,n3,n1,n3,n2,n3,n1,n3,x16,intm, 1.000)
c
!       allocate(h2(n2+1:n3,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       call reorder14235678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n2,n3,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,n0,n1,t4d,h2)
!       allocate(z16(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1,
!     & n2+1:n3,n1+1:n3))
!       i1=k3*k4
!       i2=k1*k2*k2*k2*k4*k4
!       i3=k3*k4
!       call egemm(i1,i2,i3,x16,h2,z16)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do f=n2+1,n3
             sum=sum
     &     + x16(f,e,d,a)*t4d(f,c,b,e,l,k,j,i)      !fedafcbelkji    (+1.000)
     &     - x16(f,e,c,a)*t4d(f,d,b,e,l,k,j,i)      !fecafdbelkji    (-1.000)
     &     + x16(f,e,b,a)*t4d(f,d,c,e,l,k,j,i)      !febafdcelkji    (+1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23567814(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z16, 1.000)
!       call sum13567824(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z16,-1.000)
!       call sum12567834(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z16, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z16(c,b,l,k,j,i,d,a)       ! 23567814 (+1.000)
!     & -z16(d,b,l,k,j,i,c,a)       ! 13567824 (-1.000)
!     & +z16(d,c,l,k,j,i,b,a)       ! 12567834 (+1.000)
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
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(q26(n2+1:n3,n0+1:n2))
       i1=k2*k4
       i3=k1*k3
       call egemm1(i1,i3,d1,b2,q26)
       deallocate(d1)
       deallocate(b2)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,q26,b1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s121(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
       i1=k4
       i2=k2*k4*k4
       i3=k2
       call egemm(i1,i2,i3,b1,d2,s121)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n2,n3,n2,n3,n2,n3,n0,n2,x4,s121,-1.000)
       deallocate(s121)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,q26,b1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s118(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,b1,d2,s118)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n2,n3,n2,n3,n1,n3,n0,n1,x6,s118,-1.000)
       deallocate(s118)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,q26,b1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q27(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,b1,b2,q27)
       deallocate(b1)
       deallocate(b2)
       deallocate(q26)
c
       call sum21(n2,n3,n2,n3,x11,q27,-1.000)
       deallocate(q27)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s130(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s130)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder2431(n0,n2,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n0,n2,n0,n2,s130,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder142356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
       allocate(u88(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k2*k4*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u88)
       deallocate(d1)
       deallocate(f2)
c
      call sum234516(n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,x33,u88, 1.000)
       deallocate(u88)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z89(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k2*k4*k4
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x33,d2,z89)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - x33(n,c,b,l,k,j)*t2b(d,a,n,i)          !ncblkjdani      (-1.000)
     &     + x33(n,d,b,l,k,j)*t2b(c,a,n,i)          !ndblkjcani      (+1.000)
     &     - x33(n,d,c,l,k,j)*t2b(b,a,n,i)          !ndclkjbani      (-1.000)
     &     + x33(n,c,b,l,j,k)*t2b(d,a,n,i)          !ncbljkdani      (+1.000)
     &     - x33(n,d,b,l,j,k)*t2b(c,a,n,i)          !ndbljkcani      (-1.000)
     &     + x33(n,d,c,l,j,k)*t2b(b,a,n,i)          !ndcljkbani      (+1.000)
     &     - x33(n,c,b,k,j,l)*t2b(d,a,n,i)          !ncbkjldani      (-1.000)
     &     + x33(n,d,b,k,j,l)*t2b(c,a,n,i)          !ndbkjlcani      (+1.000)
     &     - x33(n,d,c,k,j,l)*t2b(b,a,n,i)          !ndckjlbani      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14823567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z89,-1.000)
!       call sum24813567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z89, 1.000)
!       call sum34812567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z89,-1.000)
!       call sum14823576(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z89, 1.000)
!       call sum24813576(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z89,-1.000)
!       call sum34812576(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z89, 1.000)
!       call sum14823675(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z89,-1.000)
!       call sum24813675(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z89, 1.000)
!       call sum34812675(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z89,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z89(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & +z89(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z89(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & +z89(d,a,i,c,b,l,j,k)       ! 14823576 (+1.000)
!     & -z89(c,a,i,d,b,l,j,k)       ! 24813576 (-1.000)
!     & +z89(b,a,i,d,c,l,j,k)       ! 34812576 (+1.000)
!     & -z89(d,a,i,c,b,k,j,l)       ! 14823675 (-1.000)
!     & +z89(c,a,i,d,b,k,j,l)       ! 24813675 (+1.000)
!     & -z89(b,a,i,d,c,k,j,l)       ! 34812675 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z89)
       deallocate(x33)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder3214(n0,n2,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n0,n2,n0,n2,s130,d1)
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
!       allocate(h2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       call reorder51234678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n2,n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t4d,h2)
!       allocate(z10(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n1,n0+1:n2))
!       i1=k2
!       i2=k1*k2*k2*k3*k4*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x10,h2,z10)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     - x10(m,j)*t4d(d,c,b,a,m,l,k,i)          !mjdcbamlki      (-1.000)
     &     + x10(m,k)*t4d(d,c,b,a,m,l,j,i)          !mkdcbamlji      (+1.000)
     &     - x10(m,l)*t4d(d,c,b,a,m,k,j,i)          !mldcbamkji      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12345687(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z10,-1.000)
!       call sum12345786(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z10, 1.000)
!       call sum12346785(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z10,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z10(d,c,b,a,l,k,i,j)       ! 12345687 (-1.000)
!     & +z10(d,c,b,a,l,j,i,k)       ! 12345786 (+1.000)
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
     & n0,n2,n2,n3,n0,n2,n0,n2,s130,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s132(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s132)
       deallocate(d1)
       deallocate(b2)
c
       call sum3241(n0,n2,n2,n3,n2,n3,n0,n2,x18,s132,-1.000)
c
       call sumx3142(n0,n3,n0,n2,n2,n3,n2,n3,n0,n2,x18,intb, 1.000)
c
!       allocate(h2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       call reorder51234678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n2,n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t4d,h2)
!       allocate(z18(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,
!     & n2+1:n3,n0+1:n2))
!       i1=k2*k4
!       i2=k1*k2*k2*k3*k4*k4
!       i3=k4*k2
!       call egemm(i1,i2,i3,x18,h2,z18)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x18(m,e,d,j)*t4d(e,c,b,a,m,l,k,i)      !medjecbamlki    (+1.000)
     &     - x18(m,e,c,j)*t4d(e,d,b,a,m,l,k,i)      !mecjedbamlki    (-1.000)
     &     + x18(m,e,b,j)*t4d(e,d,c,a,m,l,k,i)      !mebjedcamlki    (+1.000)
     &     - x18(m,e,d,k)*t4d(e,c,b,a,m,l,j,i)      !medkecbamlji    (-1.000)
     &     + x18(m,e,c,k)*t4d(e,d,b,a,m,l,j,i)      !meckedbamlji    (+1.000)
     &     - x18(m,e,b,k)*t4d(e,d,c,a,m,l,j,i)      !mebkedcamlji    (-1.000)
     &     + x18(m,e,d,l)*t4d(e,c,b,a,m,k,j,i)      !medlecbamkji    (+1.000)
     &     - x18(m,e,c,l)*t4d(e,d,b,a,m,k,j,i)      !mecledbamkji    (-1.000)
     &     + x18(m,e,b,l)*t4d(e,d,c,a,m,k,j,i)      !mebledcamkji    (+1.000)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23456817(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z18, 1.000)
!       call sum13456827(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z18,-1.000)
!       call sum12456837(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z18, 1.000)
!       call sum23457816(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z18,-1.000)
!       call sum13457826(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z18, 1.000)
!       call sum12457836(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z18,-1.000)
!       call sum23467815(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z18, 1.000)
!       call sum13467825(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z18,-1.000)
!       call sum12467835(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z18, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z18(c,b,a,l,k,i,d,j)       ! 23456817 (+1.000)
!     & -z18(d,b,a,l,k,i,c,j)       ! 13456827 (-1.000)
!     & +z18(d,c,a,l,k,i,b,j)       ! 12456837 (+1.000)
!     & -z18(c,b,a,l,j,i,d,k)       ! 23457816 (-1.000)
!     & +z18(d,b,a,l,j,i,c,k)       ! 13457826 (+1.000)
!     & -z18(d,c,a,l,j,i,b,k)       ! 12457836 (-1.000)
!     & +z18(c,b,a,k,j,i,d,l)       ! 23467815 (+1.000)
!     & -z18(d,b,a,k,j,i,c,l)       ! 13467825 (-1.000)
!     & +z18(d,c,a,k,j,i,b,l)       ! 12467835 (+1.000)
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
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,s132,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s160(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s160)
       deallocate(d1)
       deallocate(b2)
       deallocate(s132)
c
       call sum2134(n2,n3,n2,n3,n2,n3,n0,n2,x4,s160, 1.000)
       deallocate(s160)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder2314(n0,n2,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n0,n2,n0,n2,s130,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s131(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s131)
       deallocate(d1)
       deallocate(b2)
       deallocate(s130)
c
       call sum3241(n0,n2,n0,n2,n0,n2,n0,n2,x17,s131, 1.000)
c
       call sumx2143(n0,n3,n0,n2,n0,n2,n0,n2,n0,n2,x17,intb, 1.000)
c
!       allocate(h2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,i
!     & n0+1:n2,n0+1:n1))
!       call reorder56123478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t4d,h2)
!       allocate(z17(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2
!       i2=k1*k2*k3*k4*k4*k4
!       i3=k2*k2
!       call egemm(i1,i2,i3,x17,h2,z17)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2;do n=n0+1,n2
             sum=sum
     &     + (x17(n,m,k,j)*t4d(d,c,b,a,n,m,l,i)        !nmkjdcbanmli    (+0.500)
     &     - x17(n,m,l,j)*t4d(d,c,b,a,n,m,k,i)         !nmljdcbanmki    (-0.500)
     &     + x17(n,m,l,k)*t4d(d,c,b,a,n,m,j,i))/2.0d0  !nmlkdcbanmji    (+0.500)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12345867(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z17, 0.500)
!       call sum12346857(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z17,-0.500)
!       call sum12347856(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z17, 0.500)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z17(d,c,b,a,l,i,k,j)      ! 12345867 (+0.500)
!     & -z17(d,c,b,a,k,i,l,j)       ! 12346857 (-0.500)
!     & +z17(d,c,b,a,j,i,l,k))/2.0d0! 12347856 (+0.500)
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
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder2413(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s131,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(u118(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,d1,d2,u118)
       deallocate(d1)
       deallocate(d2)
c
      call sum236145(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x24,u118,1.000)
       deallocate(u118)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z223(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x24,d2,z223)
!       deallocate(d2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - x24(n,d,a,k,j,i)*t2c(c,b,n,l)          !ndakjicbnl      (-1.000)
     &     + x24(n,c,a,k,j,i)*t2c(d,b,n,l)          !ncakjidbnl      (+1.000)
     &     - x24(n,b,a,k,j,i)*t2c(d,c,n,l)          !nbakjidcnl      (-1.000)
     &     + x24(n,d,a,l,j,i)*t2c(c,b,n,k)          !ndaljicbnk      (+1.000)
     &     - x24(n,c,a,l,j,i)*t2c(d,b,n,k)          !ncaljidbnk      (-1.000)
     &     + x24(n,b,a,l,j,i)*t2c(d,c,n,k)          !nbaljidcnk      (+1.000)
     &     - x24(n,b,a,l,k,i)*t2c(d,c,n,j)          !nbalkidcnj      (-1.000)
     &     + x24(n,c,a,l,k,i)*t2c(d,b,n,j)          !ncalkidbnj      (+1.000)
     &     - x24(n,d,a,l,k,i)*t2c(c,b,n,j)          !ndalkicbnj      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23514678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z223,-1.000)
!       call sum13524678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z223, 1.000)
!       call sum12534678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z223,-1.000)
!       call sum23614578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z223, 1.000)
!       call sum13624578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z223,-1.000)
!       call sum12634578(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z223, 1.000)
!       call sum12734568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z223,-1.000)
!       call sum13724568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z223, 1.000)
!       call sum23714568(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z223,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z223(c,b,l,d,a,k,j,i)       ! 23514678 (-1.000)
!     & +z223(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & -z223(d,c,l,b,a,k,j,i)       ! 12534678 (-1.000)
!     & +z223(c,b,k,d,a,l,j,i)       ! 23614578 (+1.000)
!     & -z223(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!     & +z223(d,c,k,b,a,l,j,i)       ! 12634578 (+1.000)
!     & -z223(d,c,j,b,a,l,k,i)       ! 12734568 (-1.000)
!     & +z223(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!     & -z223(c,b,j,d,a,l,k,i)       ! 23714568 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z223)
       deallocate(x24)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder2413(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s131,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s159(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s159)
       deallocate(d1)
       deallocate(b2)
       deallocate(s131)
c
       call sum2134(n0,n2,n2,n3,n0,n2,n0,n2,x3,s159,-1.000)
       deallocate(s159)
c
       call sumx2143(n0,n3,n0,n2,n2,n3,n0,n2,n0,n2,x3,intb, 1.000)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
!       allocate(z3(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k1*k2*k3*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x3,f2,z3)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + x3(m,d,k,j)*t3c(c,b,a,m,l,i)           !mdkjcbamli      (+1.000)
     &     - x3(m,c,k,j)*t3c(d,b,a,m,l,i)           !mckjdbamli      (-1.000)
     &     + x3(m,b,k,j)*t3c(d,c,a,m,l,i)           !mbkjdcamli      (+1.000)
     &     - x3(m,d,l,j)*t3c(c,b,a,m,k,i)           !mdljcbamki      (-1.000)
     &     + x3(m,c,l,j)*t3c(d,b,a,m,k,i)           !mcljdbamki      (+1.000)
     &     - x3(m,b,l,j)*t3c(d,c,a,m,k,i)           !mbljdcamki      (-1.000)
     &     + x3(m,d,l,k)*t3c(c,b,a,m,j,i)           !mdlkcbamji      (+1.000)
     &     - x3(m,c,l,k)*t3c(d,b,a,m,j,i)           !mclkdbamji      (-1.000)
     &     + x3(m,b,l,k)*t3c(d,c,a,m,j,i)           !mblkdcamji      (+1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23458167(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z3, 1.000)
!       call sum13458267(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z3,-1.000)
!       call sum12458367(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z3, 1.000)
!       call sum23468157(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z3,-1.000)
!       call sum13468257(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z3, 1.000)
!       call sum12468357(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z3,-1.000)
!       call sum23478156(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z3, 1.000)
!       call sum13478256(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z3,-1.000)
!       call sum12478356(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z3, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z3(c,b,a,l,i,d,k,j)       ! 23458167 (+1.000)
!     & -z3(d,b,a,l,i,c,k,j)       ! 13458267 (-1.000)
!     & +z3(d,c,a,l,i,b,k,j)       ! 12458367 (+1.000)
!     & -z3(c,b,a,k,i,d,l,j)       ! 23468157 (-1.000)
!     & +z3(d,b,a,k,i,c,l,j)       ! 13468257 (+1.000)
!     & -z3(d,c,a,k,i,b,l,j)       ! 12468357 (-1.000)
!     & +z3(c,b,a,j,i,d,l,k)       ! 23478156 (+1.000)
!     & -z3(d,b,a,j,i,c,l,k)       ! 13478256 (-1.000)
!     & +z3(d,c,a,j,i,b,l,k)       ! 12478356 (+1.000)
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
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s133(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s133)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3421(n2,n3,n2,n3,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n2,n3,s133,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q30(n2+1:n3,n2+1:n3))
       i1=k4*k4
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q30)
       deallocate(d1)
       deallocate(b2)
c
       x11=x11+q30
       deallocate(q30)
c
       call sumx21(0,n3,n2,n3,n2,n3,x11,fockb, 1.000)
c
!       allocate(h2(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       call reorder12345678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,t4d,h2)
!       allocate(z11(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,
!     & n0+1:n1,n2+1:n3))
!       i1=k4
!       i2=k1*k2*k2*k2*k3*k4*k4
!       i3=k4
!       call egemm(i1,i2,i3,x11,h2,z11)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     + x11(e,d)*t4d(e,c,b,a,l,k,j,i)          !edecbalkji      (+1.000)
     &     - x11(e,c)*t4d(e,d,b,a,l,k,j,i)          !ecedbalkji      (-1.000)
     &     + x11(e,b)*t4d(e,d,c,a,l,k,j,i)          !ebedcalkji      (+1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23456781(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z11, 1.000)
!       call sum13456782(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z11,-1.000)
!       call sum12456783(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z11, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z11(c,b,a,l,k,j,i,d)       ! 23456781 (+1.000)
!     & -z11(d,b,a,l,k,j,i,c)       ! 13456782 (-1.000)
!     & +z11(d,c,a,l,k,j,i,b)       ! 12456783 (+1.000)
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
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder4231(n2,n3,n2,n3,n2,n3,n0,n2,
     & n0,n2,n2,n3,n2,n3,n2,n3,s133,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s134(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s134)
       deallocate(d1)
       deallocate(b2)
       deallocate(s133)
c
       call sum3124(n2,n3,n2,n3,n2,n3,n2,n3,x19,s134, 1.000)
       deallocate(s134)
c
       call sumx4321(n0,n3,n2,n3,n2,n3,n2,n3,n2,n3,x19,intb, 1.000)
c
!       allocate(h2(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       call reorder12345678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,t4d,h2)
!       allocate(z19(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1,
!     & n2+1:n3,n2+1:n3))
!       i1=k4*k4
!       i2=k1*k2*k2*k2*k3*k4
!       i3=k4*k4
!       call egemm(i1,i2,i3,x19,h2,z19)
!       deallocate(h2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do f=n2+1,n3
             sum=sum
     &     + (x19(f,e,d,c)*t4d(f,e,b,a,l,k,j,i)       !fedcfebalkji    (+0.500)
     &     - x19(f,e,d,b)*t4d(f,e,c,a,l,k,j,i)        !fedbfecalkji    (-0.500)
     &     + x19(f,e,c,b)*t4d(f,e,d,a,l,k,j,i))/2.0d0 !fecbfedalkji    (+0.500)
             enddo;enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34567812(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z19, 0.500)
!       call sum24567813(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z19,-0.500)
!       call sum14567823(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z19, 0.500)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +(z19(b,a,l,k,j,i,d,c)      ! 34567812 (+0.500)
!     & -z19(c,a,l,k,j,i,d,b)       ! 24567813 (-0.500)
!     & +z19(d,a,l,k,j,i,c,b))/2.0d0! 14567823 (+0.500)
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
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q31(n1+1:n3,n0+1:n1))
       i1=k1*k3
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q31)
       deallocate(d1)
       deallocate(b2)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,q31,b1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(s144(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
       i1=k3
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s144)
       deallocate(b1)
       deallocate(d2)
       deallocate(q31)
c
       call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x2,s144,-1.000)
       deallocate(s144)
c
       call sumx3214(n0,n3,n1,n3,n2,n3,n1,n3,n0,n2,x2,intm, 1.000)
c
!       allocate(f2(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder312456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
!       allocate(z2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n1+1:n3,n0+1:n2))
!       i1=k2*k3*k4
!       i2=k1*k2*k2*k4*k4
!       i3=k3
!       call egemm(i1,i2,i3,x2,f2,z2)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + x2(e,d,a,j)*t3c(c,b,e,l,k,i)           !edajcbelki      (+1.000)
     &     - x2(e,c,a,j)*t3c(d,b,e,l,k,i)           !ecajdbelki      (-1.000)
     &     + x2(e,b,a,j)*t3c(d,c,e,l,k,i)           !ebajdcelki      (+1.000)
     &     - x2(e,d,a,k)*t3c(c,b,e,l,j,i)           !edakcbelji      (-1.000)
     &     + x2(e,c,a,k)*t3c(d,b,e,l,j,i)           !ecakdbelji      (+1.000)
     &     - x2(e,b,a,k)*t3c(d,c,e,l,j,i)           !ebakdcelji      (-1.000)
     &     + x2(e,d,a,l)*t3c(c,b,e,k,j,i)           !edalcbekji      (+1.000)
     &     - x2(e,c,a,l)*t3c(d,b,e,k,j,i)           !ecaldbekji      (-1.000)
     &     + x2(e,b,a,l)*t3c(d,c,e,k,j,i)           !ebaldcekji      (+1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23568147(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z2, 1.000)
!       call sum13568247(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z2,-1.000)
!       call sum12568347(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z2, 1.000)
!       call sum23578146(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z2,-1.000)
!       call sum13578246(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z2, 1.000)
!       call sum12578346(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z2,-1.000)
!       call sum23678145(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z2, 1.000)
!       call sum13678245(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z2,-1.000)
!       call sum12678345(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z2, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z2(c,b,l,k,i,d,a,j)       ! 23568147 (+1.000)
!     & -z2(d,b,l,k,i,c,a,j)       ! 13568247 (-1.000)
!     & +z2(d,c,l,k,i,b,a,j)       ! 12568347 (+1.000)
!     & -z2(c,b,l,j,i,d,a,k)       ! 23578146 (-1.000)
!     & +z2(d,b,l,j,i,c,a,k)       ! 13578246 (+1.000)
!     & -z2(d,c,l,j,i,b,a,k)       ! 12578346 (-1.000)
!     & +z2(c,b,k,j,i,d,a,l)       ! 23678145 (+1.000)
!     & -z2(d,b,k,j,i,c,a,l)       ! 13678245 (-1.000)
!     & +z2(d,c,k,j,i,b,a,l)       ! 12678345 (+1.000)
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
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q32(n2+1:n3,n0+1:n2))
       i1=k2*k4
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q32)
       deallocate(d1)
       deallocate(b2)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,q32,b1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s154(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
       i1=k4
       i2=k2*k4*k4
       i3=k2
       call egemm(i1,i2,i3,b1,d2,s154)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n2,n3,n2,n3,n2,n3,n0,n2,x4,s154,-1.000)
       deallocate(s154)
c
       call sumx3241(n0,n3,n2,n3,n2,n3,n2,n3,n0,n2,x4,intb, 1.000)
c
!       allocate(f2(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder123456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
!       allocate(z4(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k1*k2*k2*k3*k4
!       i3=k4
!       call egemm(i1,i2,i3,x4,f2,z4)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     - x4(e,d,c,j)*t3c(e,b,a,l,k,i)           !edcjebalki      (-1.000)
     &     + x4(e,d,b,j)*t3c(e,c,a,l,k,i)           !edbjecalki      (+1.000)
     &     - x4(e,c,b,j)*t3c(e,d,a,l,k,i)           !ecbjedalki      (-1.000)
     &     + x4(e,d,c,k)*t3c(e,b,a,l,j,i)           !edckebalji      (+1.000)
     &     - x4(e,d,b,k)*t3c(e,c,a,l,j,i)           !edbkecalji      (-1.000)
     &     + x4(e,c,b,k)*t3c(e,d,a,l,j,i)           !ecbkedalji      (+1.000)
     &     - x4(e,d,c,l)*t3c(e,b,a,k,j,i)           !edclebakji      (-1.000)
     &     + x4(e,d,b,l)*t3c(e,c,a,k,j,i)           !edblecakji      (+1.000)
     &     - x4(e,c,b,l)*t3c(e,d,a,k,j,i)           !ecbledakji      (-1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34568127(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z4,-1.000)
!       call sum24568137(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z4, 1.000)
!       call sum14568237(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z4,-1.000)
!       call sum34578126(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z4, 1.000)
!       call sum24578136(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z4,-1.000)
!       call sum14578236(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z4, 1.000)
!       call sum34678125(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z4,-1.000)
!       call sum24678135(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z4, 1.000)
!       call sum14678235(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z4,-1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & -z4(b,a,l,k,i,d,c,j)       ! 34568127 (-1.000)
!     & +z4(c,a,l,k,i,d,b,j)       ! 24568137 (+1.000)
!     & -z4(d,a,l,k,i,c,b,j)       ! 14568237 (-1.000)
!     & +z4(b,a,l,j,i,d,c,k)       ! 34578126 (+1.000)
!     & -z4(c,a,l,j,i,d,b,k)       ! 24578136 (-1.000)
!     & +z4(d,a,l,j,i,c,b,k)       ! 14578236 (+1.000)
!     & -z4(b,a,k,j,i,d,c,l)       ! 34678125 (-1.000)
!     & +z4(c,a,k,j,i,d,b,l)       ! 24678135 (+1.000)
!     & -z4(d,a,k,j,i,c,b,l)       ! 14678235 (-1.000)
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
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,q32,b1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s148(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,b1,d2,s148)
       deallocate(b1)
       deallocate(d2)
       deallocate(q32)
c
       call sum2341(n2,n3,n2,n3,n1,n3,n0,n1,x6,s148,-1.000)
       deallocate(s148)
c
       call sumx3241(n0,n3,n2,n3,n2,n3,n1,n3,n0,n1,x6,intm, 1.000)
c
!       allocate(f2(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
!       call reorder123456(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,t3d,f2)
!       allocate(z6(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3*k4
!       i2=k2*k2*k2*k4*k4
!       i3=k4
!       call egemm(i1,i2,i3,x6,f2,z6)
!       deallocate(f2)
c
       do i=n0+1,n1;do j=n0+1,n2-2;do k=j+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3;do b=n2+1,n3-2;do c=b+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     + x6(e,d,a,i)*t3d(e,c,b,l,k,j)           !edaiecblkj      (+1.000)
     &     - x6(e,c,a,i)*t3d(e,d,b,l,k,j)           !ecaiedblkj      (-1.000)
     &     + x6(e,b,a,i)*t3d(e,d,c,l,k,j)           !ebaiedclkj      (+1.000)
             enddo
             v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23567148(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z6, 1.000)
!       call sum13567248(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z6,-1.000)
!       call sum12567348(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,n0,n1,v4d,z6, 1.000)
c
!       do i=n0+1,n1
!       do j=n0+1,n2-2
!       do k=j+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3
!       do b=n2+1,n3-2
!       do c=b+1,n3-1
!       do d=c+1,n3
!c
!       v4d(d,c,b,a,l,k,j,i)=v4d(d,c,b,a,l,k,j,i)
!     & +z6(c,b,l,k,j,d,a,i)       ! 23567148 (+1.000)
!     & -z6(d,b,l,k,j,c,a,i)       ! 13567248 (-1.000)
!     & +z6(d,c,l,k,j,b,a,i)       ! 12567348 (+1.000)
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
c
       do i=n0+1,n1
       do j=n0+1,n2-2
       do k=j+1,n2-1
       do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
       do a=n1+1,n3
       do b=n2+1,n3-2
       do c=b+1,n3-1
       do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
!
!        iocca=0
!        ioccb=0
!        iunoa=0
!        iunob=0
!        if(i.gt.(n1-iactocca))iocca=iocca+1
!        if(j.gt.(n2-iactoccb))ioccb=ioccb+1
!        if(k.gt.(n2-iactoccb))ioccb=ioccb+1
!        if(l.gt.(n2-iactoccb))ioccb=ioccb+1
!        if(iocca+ioccb.lt.iactindq)cycle
!        if(a.lt.(n1+iactunoa+1))iunoa=iunoa+1
!        if(b.lt.(n2+iactunob+1))iunob=iunob+1
!        if(c.lt.(n2+iactunob+1))iunob=iunob+1
!        if(d.lt.(n2+iactunob+1))iunob=iunob+1
!        if(iunoa+iunob.lt.iactindq)cycle
!
         coeleft=fockb(d,d)
     &          +fockb(c,c)
     &          +fockb(b,b)
     &          +fockr(a,a)
     &          -fockb(l,l)
     &          -fockb(k,k)
     &          -fockb(j,j)
     &          -fockr(i,i)
     &          +shift
         t4d(d,c,b,a,l,k,j,i)=t4d(d,c,b,a,l,k,j,i)-
     & v4d(d,c,b,a,l,k,j,i)/coeleft
         t4d(d,c,b,a,l,j,k,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(d,c,b,a,j,k,l,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(d,c,b,a,j,l,k,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(d,c,b,a,k,l,j,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(d,c,b,a,k,j,l,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(d,b,c,a,l,k,j,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(d,b,c,a,l,j,k,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(d,b,c,a,j,k,l,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(d,b,c,a,j,l,k,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(d,b,c,a,k,l,j,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(d,b,c,a,k,j,l,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(b,c,d,a,l,k,j,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(b,c,d,a,l,j,k,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(b,c,d,a,j,k,l,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(b,c,d,a,j,l,k,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(b,c,d,a,k,l,j,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(b,c,d,a,k,j,l,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(b,d,c,a,l,k,j,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(b,d,c,a,l,j,k,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(b,d,c,a,j,k,l,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(b,d,c,a,j,l,k,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(b,d,c,a,k,l,j,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(b,d,c,a,k,j,l,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(c,d,b,a,l,k,j,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(c,d,b,a,l,j,k,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(c,d,b,a,j,k,l,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(c,d,b,a,j,l,k,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(c,d,b,a,k,l,j,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(c,d,b,a,k,j,l,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(c,b,d,a,l,k,j,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(c,b,d,a,l,j,k,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(c,b,d,a,j,k,l,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(c,b,d,a,j,l,k,i)= t4d(d,c,b,a,l,k,j,i)
         t4d(c,b,d,a,k,l,j,i)=-t4d(d,c,b,a,l,k,j,i)
         t4d(c,b,d,a,k,j,l,i)= t4d(d,c,b,a,l,k,j,i)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       rewind(td)
       write(td)t4d
c
       end
