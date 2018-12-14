       subroutine t4c_update(n0,n1,n2,n3,k1,k2,k3,k4,lvl,shift,
     & fockr,fockb,intr,intb,intm,t1a,t1b,t2a,t2b,t2c,t3a,t3b,t3c,t3d,
     & iactocca,iactoccb,iactunoa,iactunob,iactindq)
!    & t4a,t4b,t4c,t4d,t4e)
c
       integer a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p
       integer iactocca,iactoccb,iactunoa,iactunob,iactindq
       integer iocca,ioccb,iunoa,iunob
!       integer indocc(n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1)
!       integer indunocc(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3)
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
       real*8,allocatable::t4d(:,:,:,:,:,:,:,:)
!       real*8 t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1)
       real*8,allocatable::t4e(:,:,:,:,:,:,:,:)
!       real*8 t4e(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2)
       real*8,allocatable::v4c(:,:,:,:,:,:,:,:)
!       real*8 v4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n1,n0+1:n1)
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
       real*8,allocatable::u92(:,:,:,:,:,:)
       real*8,allocatable::s109(:,:,:,:)
       real*8,allocatable::s12(:,:,:,:)
       real*8,allocatable::u93(:,:,:,:,:,:)
       real*8,allocatable::s110(:,:,:,:)
       real*8,allocatable::q3(:,:)
       real*8,allocatable::s13(:,:,:,:)
       real*8,allocatable::u94(:,:,:,:,:,:)
       real*8,allocatable::s112(:,:,:,:)
       real*8,allocatable::s111(:,:,:,:)
       real*8,allocatable::s14(:,:,:,:)
       real*8,allocatable::u95(:,:,:,:,:,:)
       real*8,allocatable::q4(:,:)
       real*8,allocatable::s15(:,:,:,:)
       real*8,allocatable::u96(:,:,:,:,:,:)
       real*8,allocatable::u84(:,:,:,:,:,:)
       real*8,allocatable::s122(:,:,:,:)
       real*8,allocatable::s113(:,:,:,:)
       real*8,allocatable::s16(:,:,:,:)
       real*8,allocatable::u97(:,:,:,:,:,:)
       real*8,allocatable::u85(:,:,:,:,:,:)
       real*8,allocatable::s123(:,:,:,:)
       real*8,allocatable::s114(:,:,:,:)
       real*8,allocatable::s17(:,:,:,:)
       real*8,allocatable::u98(:,:,:,:,:,:)
       real*8,allocatable::u86(:,:,:,:,:,:)
       real*8,allocatable::s124(:,:,:,:)
       real*8,allocatable::q5(:,:)
       real*8,allocatable::s18(:,:,:,:)
       real*8,allocatable::u99(:,:,:,:,:,:)
       real*8,allocatable::u87(:,:,:,:,:,:)
       real*8,allocatable::s125(:,:,:,:)
       real*8,allocatable::q6(:,:)
       real*8,allocatable::s19(:,:,:,:)
       real*8,allocatable::u101(:,:,:,:,:,:)
       real*8,allocatable::u100(:,:,:,:,:,:)
       real*8,allocatable::s127(:,:,:,:)
       real*8,allocatable::s126(:,:,:,:)
       real*8,allocatable::s20(:,:,:,:)
       real*8,allocatable::u103(:,:,:,:,:,:)
       real*8,allocatable::u102(:,:,:,:,:,:)
       real*8,allocatable::s129(:,:,:,:)
       real*8,allocatable::s128(:,:,:,:)
       real*8,allocatable::s21(:,:,:,:)
       real*8,allocatable::s22(:,:,:,:)
       real*8,allocatable::s23(:,:,:,:)
       real*8,allocatable::s24(:,:,:,:)
       real*8,allocatable::s25(:,:,:,:)
       real*8,allocatable::s26(:,:,:,:)
       real*8,allocatable::s27(:,:,:,:)
       real*8,allocatable::s28(:,:,:,:)
       real*8,allocatable::s29(:,:,:,:)
       real*8,allocatable::s30(:,:,:,:)
       real*8,allocatable::s31(:,:,:,:)
       real*8,allocatable::u120(:,:,:,:,:,:)
       real*8,allocatable::u118(:,:,:,:,:,:)
       real*8,allocatable::s32(:,:,:,:)
       real*8,allocatable::u121(:,:,:,:,:,:)
       real*8,allocatable::u119(:,:,:,:,:,:)
       real*8,allocatable::q7(:,:)
       real*8,allocatable::q8(:,:)
       real*8,allocatable::s33(:,:,:,:)
       real*8,allocatable::u130(:,:,:,:,:,:)
       real*8,allocatable::u122(:,:,:,:,:,:)
       real*8,allocatable::s158(:,:,:,:)
       real*8,allocatable::s34(:,:,:,:)
       real*8,allocatable::u131(:,:,:,:,:,:)
       real*8,allocatable::u123(:,:,:,:,:,:)
       real*8,allocatable::q9(:,:)
       real*8,allocatable::s35(:,:,:,:)
       real*8,allocatable::u132(:,:,:,:,:,:)
       real*8,allocatable::u124(:,:,:,:,:,:)
       real*8,allocatable::s159(:,:,:,:)
       real*8,allocatable::s36(:,:,:,:)
       real*8,allocatable::u133(:,:,:,:,:,:)
       real*8,allocatable::u125(:,:,:,:,:,:)
       real*8,allocatable::q10(:,:)
       real*8,allocatable::s37(:,:,:,:)
       real*8,allocatable::u134(:,:,:,:,:,:)
       real*8,allocatable::s160(:,:,:,:)
       real*8,allocatable::s38(:,:,:,:)
       real*8,allocatable::u135(:,:,:,:,:,:)
       real*8,allocatable::s161(:,:,:,:)
       real*8,allocatable::q11(:,:)
       real*8,allocatable::s39(:,:,:,:)
       real*8,allocatable::u136(:,:,:,:,:,:)
       real*8,allocatable::s163(:,:,:,:)
       real*8,allocatable::s162(:,:,:,:)
       real*8,allocatable::s40(:,:,:,:)
       real*8,allocatable::u137(:,:,:,:,:,:)
       real*8,allocatable::q12(:,:)
       real*8,allocatable::u1(:,:,:,:,:,:)
       real*8,allocatable::u2(:,:,:,:,:,:)
       real*8,allocatable::u3(:,:,:,:,:,:)
       real*8,allocatable::u4(:,:,:,:,:,:)
       real*8,allocatable::u5(:,:,:,:,:,:)
       real*8,allocatable::u6(:,:,:,:,:,:)
       real*8,allocatable::s41(:,:,:,:)
       real*8,allocatable::s42(:,:,:,:)
       real*8,allocatable::u7(:,:,:,:,:,:)
       real*8,allocatable::s43(:,:,:,:)
       real*8,allocatable::s44(:,:,:,:)
       real*8,allocatable::u8(:,:,:,:,:,:)
       real*8,allocatable::s45(:,:,:,:)
       real*8,allocatable::s46(:,:,:,:)
       real*8,allocatable::u9(:,:,:,:,:,:)
       real*8,allocatable::u10(:,:,:,:,:,:)
       real*8,allocatable::u11(:,:,:,:,:,:)
       real*8,allocatable::s47(:,:,:,:)
       real*8,allocatable::u12(:,:,:,:,:,:)
       real*8,allocatable::s48(:,:,:,:)
       real*8,allocatable::u13(:,:,:,:,:,:)
       real*8,allocatable::u14(:,:,:,:,:,:)
       real*8,allocatable::u15(:,:,:,:,:,:)
       real*8,allocatable::u152(:,:,:,:,:,:)
       real*8,allocatable::s139(:,:,:,:)
       real*8,allocatable::u89(:,:,:,:,:,:)
       real*8,allocatable::s49(:,:,:,:)
       real*8,allocatable::u153(:,:,:,:,:,:)
       real*8,allocatable::s138(:,:,:,:)
       real*8,allocatable::u16(:,:,:,:,:,:)
       real*8,allocatable::s50(:,:,:,:)
       real*8,allocatable::u155(:,:,:,:,:,:)
       real*8,allocatable::s140(:,:,:,:)
       real*8,allocatable::s136(:,:,:,:)
       real*8,allocatable::q13(:,:)
       real*8,allocatable::s51(:,:,:,:)
       real*8,allocatable::u156(:,:,:,:,:,:)
       real*8,allocatable::s137(:,:,:,:)
       real*8,allocatable::q14(:,:)
       real*8,allocatable::u17(:,:,:,:,:,:)
       real*8,allocatable::u159(:,:,:,:,:,:)
       real*8,allocatable::u158(:,:,:,:,:,:)
       real*8,allocatable::u157(:,:,:,:,:,:)
       real*8,allocatable::s170(:,:,:,:)
       real*8,allocatable::u127(:,:,:,:,:,:)
       real*8,allocatable::u126(:,:,:,:,:,:)
       real*8,allocatable::u189(:,:,:,:,:,:)
       real*8,allocatable::u91(:,:,:,:,:,:)
       real*8,allocatable::u184(:,:,:,:,:,:)
       real*8,allocatable::u183(:,:,:,:,:,:)
       real*8,allocatable::u18(:,:,:,:,:,:)
       real*8,allocatable::s52(:,:,:,:)
       real*8,allocatable::u164(:,:,:,:,:,:)
       real*8,allocatable::u162(:,:,:,:,:,:)
       real*8,allocatable::s172(:,:,:,:)
       real*8,allocatable::s171(:,:,:,:)
       real*8,allocatable::u19(:,:,:,:,:,:)
       real*8,allocatable::u20(:,:,:,:,:,:)
       real*8,allocatable::u21(:,:,:,:,:,:)
       real*8,allocatable::u22(:,:,:,:,:,:)
       real*8,allocatable::u23(:,:,:,:,:,:)
       real*8,allocatable::u24(:,:,:,:,:,:)
       real*8,allocatable::u25(:,:,:,:,:,:)
       real*8,allocatable::u26(:,:,:,:,:,:)
       real*8,allocatable::u27(:,:,:,:,:,:)
       real*8,allocatable::u28(:,:,:,:,:,:)
       real*8,allocatable::u29(:,:,:,:,:,:)
       real*8,allocatable::u30(:,:,:,:,:,:)
       real*8,allocatable::s53(:,:,:,:)
       real*8,allocatable::s54(:,:,:,:)
       real*8,allocatable::u31(:,:,:,:,:,:)
       real*8,allocatable::u32(:,:,:,:,:,:)
       real*8,allocatable::s55(:,:,:,:)
       real*8,allocatable::u33(:,:,:,:,:,:)
       real*8,allocatable::u34(:,:,:,:,:,:)
       real*8,allocatable::s56(:,:,:,:)
       real*8,allocatable::u35(:,:,:,:,:,:)
       real*8,allocatable::u36(:,:,:,:,:,:)
       real*8,allocatable::s57(:,:,:,:)
       real*8,allocatable::u37(:,:,:,:,:,:)
       real*8,allocatable::u38(:,:,:,:,:,:)
       real*8,allocatable::s58(:,:,:,:)
       real*8,allocatable::u39(:,:,:,:,:,:)
       real*8,allocatable::s59(:,:,:,:)
       real*8,allocatable::s60(:,:,:,:)
       real*8,allocatable::u40(:,:,:,:,:,:)
       real*8,allocatable::s61(:,:,:,:)
       real*8,allocatable::s62(:,:,:,:)
       real*8,allocatable::s63(:,:,:,:)
       real*8,allocatable::s64(:,:,:,:)
       real*8,allocatable::u41(:,:,:,:,:,:)
       real*8,allocatable::u42(:,:,:,:,:,:)
       real*8,allocatable::s65(:,:,:,:)
       real*8,allocatable::u43(:,:,:,:,:,:)
       real*8,allocatable::s66(:,:,:,:)
       real*8,allocatable::s67(:,:,:,:)
       real*8,allocatable::u44(:,:,:,:,:,:)
       real*8,allocatable::u45(:,:,:,:,:,:)
       real*8,allocatable::s68(:,:,:,:)
       real*8,allocatable::u46(:,:,:,:,:,:)
       real*8,allocatable::s69(:,:,:,:)
       real*8,allocatable::s70(:,:,:,:)
       real*8,allocatable::u47(:,:,:,:,:,:)
       real*8,allocatable::u48(:,:,:,:,:,:)
       real*8,allocatable::s71(:,:,:,:)
       real*8,allocatable::u49(:,:,:,:,:,:)
       real*8,allocatable::u50(:,:,:,:,:,:)
       real*8,allocatable::s72(:,:,:,:)
       real*8,allocatable::u51(:,:,:,:,:,:)
       real*8,allocatable::u154(:,:,:,:,:,:)
       real*8,allocatable::s143(:,:,:,:)
       real*8,allocatable::u106(:,:,:,:,:,:)
       real*8,allocatable::u178(:,:,:,:,:,:)
       real*8,allocatable::u104(:,:,:,:,:,:)
       real*8,allocatable::u177(:,:,:,:,:,:)
       real*8,allocatable::u52(:,:,:,:,:,:)
       real*8,allocatable::s73(:,:,:,:)
       real*8,allocatable::s144(:,:,:,:)
       real*8,allocatable::s142(:,:,:,:)
       real*8,allocatable::u53(:,:,:,:,:,:)
       real*8,allocatable::u167(:,:,:,:,:,:)
       real*8,allocatable::u166(:,:,:,:,:,:)
       real*8,allocatable::u165(:,:,:,:,:,:)
       real*8,allocatable::u160(:,:,:,:,:,:)
       real*8,allocatable::s176(:,:,:,:)
       real*8,allocatable::u140(:,:,:,:,:,:)
       real*8,allocatable::u138(:,:,:,:,:,:)
       real*8,allocatable::u190(:,:,:,:,:,:)
       real*8,allocatable::u111(:,:,:,:,:,:)
       real*8,allocatable::u188(:,:,:,:,:,:)
       real*8,allocatable::u187(:,:,:,:,:,:)
       real*8,allocatable::u54(:,:,:,:,:,:)
       real*8,allocatable::u169(:,:,:,:,:,:)
       real*8,allocatable::u168(:,:,:,:,:,:)
       real*8,allocatable::u141(:,:,:,:,:,:)
       real*8,allocatable::s150(:,:,:,:)
       real*8,allocatable::u112(:,:,:,:,:,:)
       real*8,allocatable::u108(:,:,:,:,:,:)
       real*8,allocatable::u186(:,:,:,:,:,:)
       real*8,allocatable::u179(:,:,:,:,:,:)
       real*8,allocatable::s74(:,:,:,:)
       real*8,allocatable::u170(:,:,:,:,:,:)
       real*8,allocatable::u161(:,:,:,:,:,:)
       real*8,allocatable::s175(:,:,:,:)
       real*8,allocatable::s149(:,:,:,:)
       real*8,allocatable::u55(:,:,:,:,:,:)
       real*8,allocatable::s75(:,:,:,:)
       real*8,allocatable::u163(:,:,:,:,:,:)
       real*8,allocatable::s174(:,:,:,:)
       real*8,allocatable::s151(:,:,:,:)
       real*8,allocatable::s76(:,:,:,:)
       real*8,allocatable::s153(:,:,:,:)
       real*8,allocatable::s146(:,:,:,:)
       real*8,allocatable::q15(:,:)
       real*8,allocatable::u56(:,:,:,:,:,:)
       real*8,allocatable::s77(:,:,:,:)
       real*8,allocatable::s179(:,:,:,:)
       real*8,allocatable::s177(:,:,:,:)
       real*8,allocatable::s78(:,:,:,:)
       real*8,allocatable::s180(:,:,:,:)
       real*8,allocatable::s147(:,:,:,:)
       real*8,allocatable::q16(:,:)
       real*8,allocatable::s79(:,:,:,:)
       real*8,allocatable::s178(:,:,:,:)
       real*8,allocatable::s148(:,:,:,:)
       real*8,allocatable::q17(:,:)
       real*8,allocatable::q18(:,:)
       real*8,allocatable::u57(:,:,:,:,:,:)
       real*8,allocatable::u173(:,:,:,:,:,:)
       real*8,allocatable::u172(:,:,:,:,:,:)
       real*8,allocatable::u171(:,:,:,:,:,:)
       real*8,allocatable::s183(:,:,:,:)
       real*8,allocatable::u146(:,:,:,:,:,:)
       real*8,allocatable::u193(:,:,:,:,:,:)
       real*8,allocatable::u144(:,:,:,:,:,:)
       real*8,allocatable::u192(:,:,:,:,:,:)
       real*8,allocatable::u58(:,:,:,:,:,:)
       real*8,allocatable::s80(:,:,:,:)
       real*8,allocatable::u175(:,:,:,:,:,:)
       real*8,allocatable::s184(:,:,:,:)
       real*8,allocatable::s182(:,:,:,:)
       real*8,allocatable::u59(:,:,:,:,:,:)
       real*8,allocatable::u60(:,:,:,:,:,:)
       real*8,allocatable::s81(:,:,:,:)
       real*8,allocatable::s82(:,:,:,:)
       real*8,allocatable::u61(:,:,:,:,:,:)
       real*8,allocatable::u62(:,:,:,:,:,:)
       real*8,allocatable::s83(:,:,:,:)
       real*8,allocatable::u63(:,:,:,:,:,:)
       real*8,allocatable::u64(:,:,:,:,:,:)
       real*8,allocatable::s84(:,:,:,:)
       real*8,allocatable::u65(:,:,:,:,:,:)
       real*8,allocatable::s85(:,:,:,:)
       real*8,allocatable::s86(:,:,:,:)
       real*8,allocatable::u66(:,:,:,:,:,:)
       real*8,allocatable::s87(:,:,:,:)
       real*8,allocatable::s88(:,:,:,:)
       real*8,allocatable::u67(:,:,:,:,:,:)
       real*8,allocatable::u149(:,:,:,:,:,:)
       real*8,allocatable::s155(:,:,:,:)
       real*8,allocatable::u116(:,:,:,:,:,:)
       real*8,allocatable::u114(:,:,:,:,:,:)
       real*8,allocatable::u182(:,:,:,:,:,:)
       real*8,allocatable::u180(:,:,:,:,:,:)
       real*8,allocatable::u68(:,:,:,:,:,:)
       real*8,allocatable::s89(:,:,:,:)
       real*8,allocatable::s157(:,:,:,:)
       real*8,allocatable::s154(:,:,:,:)
       real*8,allocatable::u69(:,:,:,:,:,:)
       real*8,allocatable::s189(:,:,:,:)
       real*8,allocatable::u151(:,:,:,:,:,:)
       real*8,allocatable::s90(:,:,:,:)
       real*8,allocatable::u174(:,:,:,:,:,:)
       real*8,allocatable::s188(:,:,:,:)
       real*8,allocatable::u70(:,:,:,:,:,:)
       real*8,allocatable::s91(:,:,:,:)
       real*8,allocatable::s190(:,:,:,:)
       real*8,allocatable::s186(:,:,:,:)
       real*8,allocatable::q19(:,:)
       real*8,allocatable::s92(:,:,:,:)
       real*8,allocatable::s187(:,:,:,:)
       real*8,allocatable::q20(:,:)
       real*8,allocatable::u71(:,:,:,:,:,:)
       real*8,allocatable::s93(:,:,:,:)
       real*8,allocatable::s94(:,:,:,:)
       real*8,allocatable::u72(:,:,:,:,:,:)
       real*8,allocatable::u148(:,:,:,:,:,:)
       real*8,allocatable::s95(:,:,:,:)
       real*8,allocatable::u73(:,:,:,:,:,:)
       real*8,allocatable::u107(:,:,:,:,:,:)
       real*8,allocatable::s96(:,:,:,:)
       real*8,allocatable::u74(:,:,:,:,:,:)
       real*8,allocatable::u105(:,:,:,:,:,:)
       real*8,allocatable::s97(:,:,:,:)
       real*8,allocatable::s98(:,:,:,:)
       real*8,allocatable::u75(:,:,:,:,:,:)
       real*8,allocatable::u143(:,:,:,:,:,:)
       real*8,allocatable::u117(:,:,:,:,:,:)
       real*8,allocatable::s99(:,:,:,:)
       real*8,allocatable::u76(:,:,:,:,:,:)
       real*8,allocatable::u142(:,:,:,:,:,:)
       real*8,allocatable::u77(:,:,:,:,:,:)
       real*8,allocatable::s100(:,:,:,:)
       real*8,allocatable::s101(:,:,:,:)
       real*8,allocatable::u78(:,:,:,:,:,:)
       real*8,allocatable::u139(:,:,:,:,:,:)
       real*8,allocatable::u79(:,:,:,:,:,:)
       real*8,allocatable::u115(:,:,:,:,:,:)
       real*8,allocatable::s102(:,:,:,:)
       real*8,allocatable::u80(:,:,:,:,:,:)
       real*8,allocatable::u129(:,:,:,:,:,:)
       real*8,allocatable::u113(:,:,:,:,:,:)
       real*8,allocatable::s103(:,:,:,:)
       real*8,allocatable::s104(:,:,:,:)
       real*8,allocatable::s105(:,:,:,:)
       real*8,allocatable::s106(:,:,:,:)
       real*8,allocatable::u81(:,:,:,:,:,:)
       real*8,allocatable::u150(:,:,:,:,:,:)
       real*8,allocatable::s107(:,:,:,:)
       real*8,allocatable::u82(:,:,:,:,:,:)
       real*8,allocatable::u147(:,:,:,:,:,:)
       real*8,allocatable::s108(:,:,:,:)
       real*8,allocatable::u83(:,:,:,:,:,:)
       real*8,allocatable::u145(:,:,:,:,:,:)
       real*8,allocatable::s115(:,:,:,:)
       real*8,allocatable::u88(:,:,:,:,:,:)
       real*8,allocatable::q21(:,:)
       real*8,allocatable::s117(:,:,:,:)
       real*8,allocatable::s193(:,:,:,:)
       real*8,allocatable::s116(:,:,:,:)
       real*8,allocatable::u176(:,:,:,:,:,:)
       real*8,allocatable::s192(:,:,:,:)
       real*8,allocatable::s118(:,:,:,:)
       real*8,allocatable::q22(:,:)
       real*8,allocatable::s119(:,:,:,:)
       real*8,allocatable::s120(:,:,:,:)
       real*8,allocatable::u110(:,:,:,:,:,:)
       real*8,allocatable::u109(:,:,:,:,:,:)
       real*8,allocatable::u90(:,:,:,:,:,:)
       real*8,allocatable::q23(:,:)
       real*8,allocatable::s131(:,:,:,:)
       real*8,allocatable::s130(:,:,:,:)
       real*8,allocatable::u185(:,:,:,:,:,:)
       real*8,allocatable::u181(:,:,:,:,:,:)
       real*8,allocatable::s196(:,:,:,:)
       real*8,allocatable::s121(:,:,:,:)
       real*8,allocatable::s195(:,:,:,:)
       real*8,allocatable::s194(:,:,:,:)
       real*8,allocatable::s132(:,:,:,:)
       real*8,allocatable::u128(:,:,:,:,:,:)
       real*8,allocatable::s164(:,:,:,:)
       real*8,allocatable::q24(:,:)
       real*8,allocatable::s133(:,:,:,:)
       real*8,allocatable::s197(:,:,:,:)
       real*8,allocatable::s134(:,:,:,:)
       real*8,allocatable::q27(:,:)
       real*8,allocatable::s135(:,:,:,:)
       real*8,allocatable::q25(:,:)
       real*8,allocatable::s156(:,:,:,:)
       real*8,allocatable::s152(:,:,:,:)
       real*8,allocatable::q26(:,:)
       real*8,allocatable::q28(:,:)
       real*8,allocatable::s145(:,:,:,:)
       real*8,allocatable::s141(:,:,:,:)
       real*8,allocatable::s165(:,:,:,:)
       real*8,allocatable::q29(:,:)
       real*8,allocatable::s167(:,:,:,:)
       real*8,allocatable::s199(:,:,:,:)
       real*8,allocatable::s166(:,:,:,:)
       real*8,allocatable::u191(:,:,:,:,:,:)
       real*8,allocatable::s198(:,:,:,:)
       real*8,allocatable::s168(:,:,:,:)
       real*8,allocatable::q30(:,:)
       real*8,allocatable::s169(:,:,:,:)
       real*8,allocatable::q31(:,:)
       real*8,allocatable::s181(:,:,:,:)
       real*8,allocatable::s173(:,:,:,:)
       real*8,allocatable::q32(:,:)
       real*8,allocatable::s191(:,:,:,:)
       real*8,allocatable::s185(:,:,:,:)
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
       real*8,allocatable::x8(:,:,:,:)
       real*8,allocatable::z8(:,:,:,:,:,:,:,:)
       real*8,allocatable::x9(:,:,:,:)
       real*8,allocatable::z9(:,:,:,:,:,:,:,:)
       real*8,allocatable::x10(:,:)
       real*8,allocatable::z10(:,:,:,:,:,:,:,:)
       real*8,allocatable::x11(:,:)
       real*8,allocatable::z11(:,:,:,:,:,:,:,:)
       real*8,allocatable::x12(:,:)
       real*8,allocatable::z12(:,:,:,:,:,:,:,:)
       real*8,allocatable::x13(:,:)
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
       real*8,allocatable::z21(:,:,:,:,:,:,:,:)
       real*8,allocatable::x22(:,:,:,:)
       real*8,allocatable::z22(:,:,:,:,:,:,:,:)
       real*8,allocatable::x23(:,:,:,:)
       real*8,allocatable::z23(:,:,:,:,:,:,:,:)
       real*8,allocatable::x24(:,:,:,:)
       real*8,allocatable::z24(:,:,:,:,:,:,:,:)
       real*8,allocatable::x25(:,:,:,:)
       real*8,allocatable::z28(:,:,:,:,:,:,:,:)
       real*8,allocatable::x26(:,:,:,:)
       real*8,allocatable::z29(:,:,:,:,:,:,:,:)
       real*8,allocatable::z278(:,:,:,:,:,:,:,:)
       real*8,allocatable::z37(:,:,:,:,:,:,:,:)
       real*8,allocatable::x27(:,:,:,:,:,:)
       real*8,allocatable::z279(:,:,:,:,:,:,:,:)
       real*8,allocatable::z281(:,:,:,:,:,:,:,:)
       real*8,allocatable::z41(:,:,:,:,:,:,:,:)
       real*8,allocatable::x28(:,:,:,:,:,:)
       real*8,allocatable::z282(:,:,:,:,:,:,:,:)
       real*8,allocatable::x29(:,:,:,:,:,:)
       real*8,allocatable::z264(:,:,:,:,:,:,:,:)
       real*8,allocatable::x30(:,:,:,:,:,:)
       real*8,allocatable::z265(:,:,:,:,:,:,:,:)
       real*8,allocatable::x31(:,:,:,:,:,:)
       real*8,allocatable::z266(:,:,:,:,:,:,:,:)
       real*8,allocatable::x32(:,:,:,:,:,:)
       real*8,allocatable::z285(:,:,:,:,:,:,:,:)
       real*8,allocatable::x33(:,:,:,:,:,:)
       real*8,allocatable::z267(:,:,:,:,:,:,:,:)
       real*8,allocatable::x34(:,:,:,:,:,:)
       real*8,allocatable::z287(:,:,:,:,:,:,:,:)
       real*8,allocatable::x35(:,:,:,:,:,:)
       real*8,allocatable::z286(:,:,:,:,:,:,:,:)
       real*8,allocatable::x36(:,:,:,:)
       real*8,allocatable::z56(:,:,:,:,:,:,:,:)
       real*8,allocatable::x37(:,:,:,:)
       real*8,allocatable::z57(:,:,:,:,:,:,:,:)
       real*8,allocatable::x38(:,:,:,:,:,:)
       real*8,allocatable::z334(:,:,:,:,:,:,:,:)
       real*8,allocatable::x39(:,:,:,:,:,:)
       real*8,allocatable::z332(:,:,:,:,:,:,:,:)
       real*8,allocatable::x40(:,:,:,:,:,:)
       real*8,allocatable::z350(:,:,:,:,:,:,:,:)
       real*8,allocatable::z352(:,:,:,:,:,:,:,:)
       real*8,allocatable::z71(:,:,:,:,:,:,:,:)
       real*8,allocatable::z355(:,:,:,:,:,:,:,:)
       real*8,allocatable::z75(:,:,:,:,:,:,:,:)
       real*8,allocatable::x41(:,:,:,:,:,:)
       real*8,allocatable::z85(:,:,:,:,:,:,:,:)
       real*8,allocatable::x42(:,:,:,:,:,:)
       real*8,allocatable::z88(:,:,:,:,:,:,:,:)
       real*8,allocatable::x43(:,:,:,:,:,:)
       real*8,allocatable::z91(:,:,:,:,:,:,:,:)
       real*8,allocatable::x44(:,:,:,:,:,:)
       real*8,allocatable::z92(:,:,:,:,:,:,:,:)
       real*8,allocatable::x45(:,:,:,:,:,:)
       real*8,allocatable::z93(:,:,:,:,:,:,:,:)
       real*8,allocatable::x46(:,:,:,:,:,:)
       real*8,allocatable::z98(:,:,:,:,:,:,:,:)
       real*8,allocatable::z99(:,:,:,:,:,:,:,:)
       real*8,allocatable::x47(:,:,:,:,:,:)
       real*8,allocatable::z389(:,:,:,:,:,:,:,:)
       real*8,allocatable::x48(:,:,:,:,:,:)
       real*8,allocatable::z392(:,:,:,:,:,:,:,:)
       real*8,allocatable::z106(:,:,:,:,:,:,:,:)
       real*8,allocatable::x49(:,:,:,:,:,:)
       real*8,allocatable::z116(:,:,:,:,:,:,:,:)
       real*8,allocatable::x50(:,:,:,:,:,:)
       real*8,allocatable::z118(:,:,:,:,:,:,:,:)
       real*8,allocatable::x51(:,:,:,:,:,:)
       real*8,allocatable::z123(:,:,:,:,:,:,:,:)
       real*8,allocatable::x52(:,:,:,:,:,:)
       real*8,allocatable::z124(:,:,:,:,:,:,:,:)
       real*8,allocatable::x53(:,:,:,:,:,:)
       real*8,allocatable::z126(:,:,:,:,:,:,:,:)
       real*8,allocatable::x54(:,:,:,:,:,:)
       real*8,allocatable::z129(:,:,:,:,:,:,:,:)
       real*8,allocatable::x55(:,:,:,:,:,:)
       real*8,allocatable::z132(:,:,:,:,:,:,:,:)
       real*8,allocatable::x56(:,:,:,:,:,:)
       real*8,allocatable::z143(:,:,:,:,:,:,:,:)
       real*8,allocatable::x57(:,:,:,:,:,:)
       real*8,allocatable::z149(:,:,:,:,:,:,:,:)
       real*8,allocatable::x58(:,:,:,:,:,:)
       real*8,allocatable::z150(:,:,:,:,:,:,:,:)
       real*8,allocatable::x59(:,:,:,:,:,:)
       real*8,allocatable::z152(:,:,:,:,:,:,:,:)
       real*8,allocatable::x60(:,:,:,:,:,:)
       real*8,allocatable::z155(:,:,:,:,:,:,:,:)
       real*8,allocatable::z161(:,:,:,:,:,:,:,:)
       real*8,allocatable::x61(:,:,:,:,:,:)
       real*8,allocatable::z401(:,:,:,:,:,:,:,:)
       real*8,allocatable::z164(:,:,:,:,:,:,:,:)
       real*8,allocatable::z165(:,:,:,:,:,:,:,:)
       real*8,allocatable::z178(:,:,:,:,:,:,:,:)
       real*8,allocatable::x62(:,:,:,:,:,:)
       real*8,allocatable::z181(:,:,:,:,:,:,:,:)
       real*8,allocatable::x63(:,:,:,:,:,:)
       real*8,allocatable::z182(:,:,:,:,:,:,:,:)
       real*8,allocatable::x64(:,:,:,:,:,:)
       real*8,allocatable::z185(:,:,:,:,:,:,:,:)
       real*8,allocatable::x65(:,:,:,:,:,:)
       real*8,allocatable::z188(:,:,:,:,:,:,:,:)
       real*8,allocatable::z189(:,:,:,:,:,:,:,:)
       real*8,allocatable::x66(:,:,:,:,:,:)
       real*8,allocatable::z194(:,:,:,:,:,:,:,:)
       real*8,allocatable::z197(:,:,:,:,:,:,:,:)
       real*8,allocatable::z200(:,:,:,:,:,:,:,:)
       real*8,allocatable::z202(:,:,:,:,:,:,:,:)
       real*8,allocatable::x67(:,:,:,:,:,:)
       real*8,allocatable::z207(:,:,:,:,:,:,:,:)
       real*8,allocatable::x68(:,:,:,:,:,:)
       real*8,allocatable::z210(:,:,:,:,:,:,:,:)
       real*8,allocatable::z211(:,:,:,:,:,:,:,:)
       real*8,allocatable::z295(:,:,:,:,:,:,:,:)
       real*8,allocatable::z212(:,:,:,:,:,:,:,:)
       real*8,allocatable::z214(:,:,:,:,:,:,:,:)
       real*8,allocatable::z216(:,:,:,:,:,:,:,:)
       real*8,allocatable::z218(:,:,:,:,:,:,:,:)
       real*8,allocatable::z224(:,:,:,:,:,:,:,:)
       real*8,allocatable::z232(:,:,:,:,:,:,:,:)
       real*8,allocatable::z233(:,:,:,:,:,:,:,:)
       real*8,allocatable::z234(:,:,:,:,:,:,:,:)
       real*8,allocatable::z235(:,:,:,:,:,:,:,:)
c
!       allocate(t4a(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       allocate(t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       allocate(t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       allocate(t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
!       allocate(t4e(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
c
       allocate(indocc(n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       allocate(indunocc(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3))
       allocate(v4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,
     & n0+1:n1,n0+1:n1))
c
       indocc=0
       indunocc=0
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        iocca=0
        ioccb=0
        if(i.gt.(n1-iactocca))iocca=iocca+1
        if(j.gt.(n1-iactocca))iocca=iocca+1
        if(k.gt.(n2-iactoccb))ioccb=ioccb+1
        if(l.gt.(n2-iactoccb))ioccb=ioccb+1
        if(iocca+ioccb.lt.iactindq)indocc(l,k,j,i)=1
       enddo;enddo;enddo;enddo
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           iunoa=0
           iunob=0
           if(a.lt.(n1+iactunoa+1))iunoa=iunoa+1
           if(b.lt.(n1+iactunoa+1))iunoa=iunoa+1
           if(c.lt.(n2+iactunob+1))iunob=iunob+1
           if(d.lt.(n2+iactunob+1))iunob=iunob+1
           if(iunoa+iunob.lt.iactindq)indunocc(d,c,b,a)=1
          enddo;enddo;enddo;enddo
c
!       rewind(ta)
       rewind(tb)
       rewind(tc)
       rewind(td)
!       rewind(te)
!       read(ta)t4a
       read(tb)t4b
       read(tc)t4c
       read(td)t4d
!       read(te)t4e
c
       v4c=0.0d0
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
       allocate(x5(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       x5=0.0d0
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x5,s3,-1.000)
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
       allocate(x25(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       x25=0.0d0
       call sum3124(n0,n1,n1,n3,n0,n1,n0,n1,x25,s4, 1.000)
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
       allocate(x26(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       x26=0.0d0
       call sum3124(n1,n3,n1,n3,n1,n3,n0,n1,x26,s5, 1.000)
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
       allocate(x6(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       x6=0.0d0
       call sum4123(n1,n3,n1,n3,n1,n3,n0,n1,x6,s6, 1.000)
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
       allocate(x7(n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
       x7=0.0d0
       call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x7,s7,-1.000)
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
       allocate(x8(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       x8=0.0d0
       call sum3124(n2,n3,n2,n3,n1,n3,n0,n1,x8,s8,-1.000)
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
       call sum4123(n0,n2,n1,n3,n0,n2,n0,n1,x7,s9, 1.000)
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
       call sum4123(n2,n3,n2,n3,n1,n3,n0,n1,x8,s10, 1.000)
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
       allocate(x10(n0+1:n1,n0+1:n1))
       x10=0.0d0
       call sum21(n0,n1,n0,n1,x10,q1, 1.000)
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
       allocate(x11(n1+1:n3,n1+1:n3))
       x11=0.0d0
       call sum21(n1,n3,n1,n3,x11,q2,-1.000)
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
!       allocate(h2(n0+1:n1,n0+1:n1,n2+1:n3,n2+1:n3,
!     & n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
!       call reorder78123456(n2,n3,n2,n3,n1,n3,n1,n3,
!     & n0,n2,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,t4c,h2)
!       allocate(z37(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
!       i1=k1*k1
!       i2=k2*k2*k3*k3*k4*k4
!       i3=k1*k1
!       call egemm(i1,i2,i3,d1,h2,z37)
!       deallocate(d1)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum
     &     + (s11(j,m,i,n)*t4c(d,c,b,a,l,k,n,m)       !jmindcbalknm    (+0.500)
     &     - s11(i,m,j,n)*t4c(d,c,b,a,l,k,n,m))/2.0d0 !imjndcbalknm    (-0.500)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       v4c=v4c+0.500*z37
!       call sum12345687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z37,-0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z37(d,c,b,a,l,k,j,i)      ! 12345678 (+0.500)
!     & -z37(d,c,b,a,l,k,i,j))/2.0d0! 12345687 (-0.500)
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
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s11,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u92(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u92)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder412356(n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,u92,f1)
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z278(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,
!     & n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k4
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z278)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     - u92(c,a,l,n,j,i)*t2b(d,b,k,n)          !calnjidbkn      (-1.000)
     &     - u92(d,a,k,n,j,i)*t2b(c,b,l,n)          !daknjicbln      (-1.000)
     &     + u92(d,a,l,n,j,i)*t2b(c,b,k,n)          !dalnjicbkn      (+1.000)
     &     + u92(c,a,k,n,j,i)*t2b(d,b,l,n)          !caknjidbln      (+1.000)
     &     + u92(c,a,l,n,i,j)*t2b(d,b,k,n)          !calnijdbkn      (+1.000)
     &     + u92(d,a,k,n,i,j)*t2b(c,b,l,n)          !daknijcbln      (+1.000)
     &     - u92(d,a,l,n,i,j)*t2b(c,b,k,n)          !dalnijcbkn      (-1.000)
     &     - u92(c,a,k,n,i,j)*t2b(d,b,l,n)          !caknijdbln      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13624578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z278,-1.000)
!       call sum23514678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z278,-1.000)
!       call sum23614578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z278, 1.000)
!       call sum13524678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z278, 1.000)
!       call sum13624587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z278, 1.000)
!       call sum23514687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z278, 1.000)
!       call sum23614587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z278,-1.000)
!       call sum13524687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z278,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z278(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!     & -z278(c,b,l,d,a,k,j,i)       ! 23514678 (-1.000)
!     & +z278(c,b,k,d,a,l,j,i)       ! 23614578 (+1.000)
!     & +z278(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & +z278(d,b,k,c,a,l,i,j)       ! 13624587 (+1.000)
!     & +z278(c,b,l,d,a,k,i,j)       ! 23514687 (+1.000)
!     & -z278(c,b,k,d,a,l,i,j)       ! 23614587 (-1.000)
!     & -z278(d,b,l,c,a,k,i,j)       ! 13524687 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z278)
       deallocate(u92)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s11,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s109(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s109)
       deallocate(d1)
       deallocate(b2)
       deallocate(s11)
c
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x25,s109,-1.000)
       deallocate(s109)
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
       allocate(x15(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       x15=0.0d0
       call sum3241(n0,n1,n1,n3,n1,n3,n0,n1,x15,s12,-1.000)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder2413(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s12,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u93(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u93)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x27(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x27=0.0d0
       call sum245136(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x27,u93,1.000)
       deallocate(u93)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4213(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s12,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s110(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s110)
       deallocate(d1)
       deallocate(b2)
       deallocate(s12)
c
       call sum2134(n1,n3,n1,n3,n1,n3,n0,n1,x6,s110, 1.000)
       deallocate(s110)
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
       call sum21(n0,n1,n0,n1,x10,q3,-1.000)
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
       call sum4231(n0,n1,n1,n3,n1,n3,n0,n1,x15,s13, 1.000)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n1,n3,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s13,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u94(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u94)
       deallocate(d1)
       deallocate(d2)
c
       call sum245136(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x27,u94,-1.000)
       deallocate(u94)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n1,n1,n3,n1,n3,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s13,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s112(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s112)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n1,n3,n1,n3,n1,n3,n0,n1,x26,s112, 1.000)
       deallocate(s112)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n1,n3,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s13,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s111(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s111)
       deallocate(d1)
       deallocate(b2)
       deallocate(s13)
c
       call sum3124(n0,n1,n1,n3,n0,n1,n0,n1,x5,s111, 1.000)
       deallocate(s111)
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
!       allocate(h2(n1+1:n3,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       call reorder34125678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,n0,n1,t4c,h2)
!       allocate(z41(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1,
!     & n1+1:n3,n1+1:n3))
!       i1=k3*k3
!       i2=k1*k1*k2*k2*k4*k4
!       i3=k3*k3
!       call egemm(i1,i2,i3,d1,h2,z41)
!       deallocate(d1)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do f=n1+1,n3
             sum=sum
     &     + (s14(a,f,e,b)*t4c(d,c,f,e,l,k,j,i)       !afebdcfelkji    (+0.500)
     &     - s14(b,f,e,a)*t4c(d,c,f,e,l,k,j,i))/2.0d0 !bfeadcfelkji    (-0.500)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12567834(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z41, 0.500)
!       call sum12567843(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z41,-0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z41(d,c,l,k,j,i,b,a)      ! 12567834 (+0.500)
!     & -z41(d,c,l,k,j,i,a,b))/2.0d0! 12567843 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z41)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder3241(n1,n3,n1,n3,n1,n3,n1,n3,
     & n1,n3,n1,n3,n1,n3,n1,n3,s14,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u95(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u95)
       deallocate(d1)
       deallocate(d2)
       deallocate(s14)
c
!       allocate(f1(n1+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder415623(n2,n3,n0,n2,n0,n1,n1,n3,n1,n3,n1,n3,
!     & n1,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,u95,f1)
!       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
!       allocate(z281(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k3*k4
!       i2=k1*k2*k4
!       i3=k3
!       call egemm(i1,i2,i3,f1,d2,z281)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3
             sum=sum
     &     - u95(d,k,i,f,b,a)*t2b(c,f,l,j)          !dkifbacflj      (-1.000)
     &     + u95(d,k,i,f,a,b)*t2b(c,f,l,j)          !dkifabcflj      (+1.000)
     &     + u95(c,k,i,f,b,a)*t2b(d,f,l,j)          !ckifbadflj      (+1.000)
     &     - u95(c,k,i,f,a,b)*t2b(d,f,l,j)          !ckifabdflj      (-1.000)
     &     + u95(d,l,i,f,b,a)*t2b(c,f,k,j)          !dlifbacfkj      (+1.000)
     &     - u95(d,l,i,f,a,b)*t2b(c,f,k,j)          !dlifabcfkj      (-1.000)
     &     - u95(c,l,i,f,b,a)*t2b(d,f,k,j)          !clifbadfkj      (-1.000)
     &     + u95(c,l,i,f,a,b)*t2b(d,f,k,j)          !clifabdfkj      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum25713468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z281,-1.000)
!       call sum25714368(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z281, 1.000)
!       call sum15723468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z281, 1.000)
!       call sum15724368(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z281,-1.000)
!       call sum26713458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z281, 1.000)
!       call sum26714358(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z281,-1.000)
!       call sum16723458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z281,-1.000)
!       call sum16724358(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z281, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z281(c,l,j,d,b,a,k,i)       ! 25713468 (-1.000)
!     & +z281(c,l,j,d,a,b,k,i)       ! 25714368 (+1.000)
!     & +z281(d,l,j,c,b,a,k,i)       ! 15723468 (+1.000)
!     & -z281(d,l,j,c,a,b,k,i)       ! 15724368 (-1.000)
!     & +z281(c,k,j,d,b,a,l,i)       ! 26713458 (+1.000)
!     & -z281(c,k,j,d,a,b,l,i)       ! 26714358 (-1.000)
!     & -z281(d,k,j,c,b,a,l,i)       ! 16723458 (-1.000)
!     & +z281(d,k,j,c,a,b,l,i)       ! 16724358 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z281)
       deallocate(u95)
c
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
       x11=x11-q4
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
       allocate(x17(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       x17=0.0d0
       call sum4231(n0,n2,n0,n1,n0,n2,n0,n1,x17,s15, 1.000)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder2431(n0,n1,n0,n1,n0,n2,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s15,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u96(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u96)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x28(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x28=0.0d0
       call sum234156(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x28,u96,1.000)
       deallocate(u96)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder2431(n0,n1,n0,n1,n0,n2,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s15,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u84(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u84)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x29(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x29=0.0d0
       call sum235146(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x29,u84,1.000)
       deallocate(u84)
c
       allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       call reorder4231(n0,n1,n0,n1,n0,n2,n0,n2,
     & n0,n2,n0,n1,n0,n2,n0,n1,s15,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s122(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s122)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s122,-1.000)
       deallocate(s122)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder2431(n0,n1,n0,n1,n0,n2,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s15,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s113(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s113)
       deallocate(d1)
       deallocate(b2)
       deallocate(s15)
c
       call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x7,s113,-1.000)
       deallocate(s113)
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
       allocate(x18(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       x18=0.0d0
       call sum4231(n0,n1,n2,n3,n2,n3,n0,n1,x18,s16, 1.000)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s16,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u97(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u97)
       deallocate(d1)
       deallocate(d2)
c
       call sum345126(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x27,u97,1.000)
       deallocate(u97)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s16,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u85(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u85)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x30(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x30=0.0d0
       call sum345126(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x30,u85,1.000)
       deallocate(u85)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s16,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s123(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s123)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n2,n3,n0,n2,n0,n1,x1,s123, 1.000)
       deallocate(s123)
c
       allocate(d1(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       call reorder4231(n0,n1,n2,n3,n2,n3,n0,n1,
     & n0,n1,n2,n3,n2,n3,n0,n1,s16,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s114(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s114)
       deallocate(d1)
       deallocate(b2)
       deallocate(s16)
c
       call sum3124(n2,n3,n2,n3,n1,n3,n0,n1,x8,s114,-1.000)
       deallocate(s114)
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
       allocate(x19(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       x19=0.0d0
       call sum3241(n0,n2,n1,n3,n1,n3,n0,n2,x19,s17,-1.000)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder2413(n1,n3,n1,n3,n0,n2,n0,n2,
     & n1,n3,n0,n2,n1,n3,n0,n2,s17,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u98(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u98)
       deallocate(d1)
       deallocate(d2)
c
       call sum246135(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x28,u98,1.000)
       deallocate(u98)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder2413(n1,n3,n1,n3,n0,n2,n0,n2,
     & n1,n3,n0,n2,n1,n3,n0,n2,s17,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u86(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u86)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x31(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x31=0.0d0
       call sum356124(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x31,u86,1.000)
       deallocate(u86)
c
       allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4213(n1,n3,n1,n3,n0,n2,n0,n2,
     & n0,n2,n1,n3,n1,n3,n0,n2,s17,d1)
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
       deallocate(s17)
c
       call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s124, 1.000)
       deallocate(s124)
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
       allocate(x12(n0+1:n2,n0+1:n2))
       x12=0.0d0
       call sum21(n0,n2,n0,n2,x12,q5, 1.000)
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
       allocate(x20(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       x20=0.0d0
       call sum4123(n2,n3,n1,n3,n2,n3,n1,n3,x20,s18,-1.000)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       call reorder2341(n1,n3,n2,n3,n1,n3,n2,n3,
     & n2,n3,n1,n3,n2,n3,n1,n3,s18,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u99(n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k3
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u99)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x32(n1+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       x32=0.0d0
       call sum456123(n1,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,x32,u99,1.000)
       deallocate(u99)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       call reorder3241(n1,n3,n2,n3,n1,n3,n2,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,s18,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u87(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k4
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u87)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x33(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       x33=0.0d0
       call sum456123(n2,n3,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x33,u87,1.000)
       deallocate(u87)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       call reorder2341(n1,n3,n2,n3,n1,n3,n2,n3,
     & n2,n3,n1,n3,n2,n3,n1,n3,s18,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s125(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s125)
       deallocate(d1)
       deallocate(b2)
       deallocate(s18)
c
       call sum4123(n1,n3,n2,n3,n1,n3,n0,n2,x2,s125,-1.000)
       deallocate(s125)
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
       allocate(x13(n2+1:n3,n2+1:n3))
       x13=0.0d0
       x13=x13+q6
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
       allocate(x24(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       x24=0.0d0
       call sum3241(n0,n2,n2,n3,n1,n3,n0,n1,x24,s19,-1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2413(n1,n3,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s19,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u101(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u101)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x34(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x34=0.0d0
      call sum245136(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x34,u101,1.000)
       deallocate(u101)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2413(n1,n3,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s19,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u100(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u100)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x35(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x35=0.0d0
      call sum345126(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x35,u100,1.000)
       deallocate(u100)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder4213(n1,n3,n2,n3,n0,n1,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n1,s19,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s127(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s127)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x8,s127, 1.000)
       deallocate(s127)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2413(n1,n3,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s19,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s126(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s126)
       deallocate(d1)
       deallocate(b2)
       deallocate(s19)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x7,s126,-1.000)
       deallocate(s126)
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
       call sum4231(n0,n2,n2,n3,n1,n3,n0,n1,x24,s20, 1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n1,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s20,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u103(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u103)
       deallocate(d1)
       deallocate(d2)
c
       call sum245136(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & x34,u103,-1.000)
       deallocate(u103)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n1,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s20,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u102(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u102)
       deallocate(d1)
       deallocate(d2)
c
       call sum345126(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x35,u102,-1.000)
       deallocate(u102)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n1,n2,n3,n1,n3,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n1,s20,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s129(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s129)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x8,s129,-1.000)
       deallocate(s129)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n1,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s20,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s128(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s128)
       deallocate(d1)
       deallocate(b2)
       deallocate(s20)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x7,s128, 1.000)
       deallocate(s128)
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
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s25(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s25)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x3(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       x3=0.0d0
       call sum2134(n0,n2,n2,n3,n0,n2,n0,n2,x3,s25,-1.000)
       deallocate(s25)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s26(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s26)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x36(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       x36=0.0d0
       call sum3124(n0,n2,n2,n3,n0,n2,n0,n2,x36,s26, 1.000)
       deallocate(s26)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s27(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s27)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x37(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       x37=0.0d0
       call sum3124(n2,n3,n2,n3,n2,n3,n0,n2,x37,s27, 1.000)
       deallocate(s27)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n2,n3,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s28(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4*k4
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s28)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x4(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       x4=0.0d0
       call sum4123(n2,n3,n2,n3,n2,n3,n0,n2,x4,s28, 1.000)
       deallocate(s28)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s29(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s29)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x7,s29, 1.000)
       deallocate(s29)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s30(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s30)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x8,s30,-1.000)
       deallocate(s30)
c
       allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n1,n3,n0,n2,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s31(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s31)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x9(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       x9=0.0d0
       call sum3124(n0,n1,n1,n3,n2,n3,n0,n2,x9,s31,-1.000)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3214(n2,n3,n0,n1,n1,n3,n0,n2,
     & n1,n3,n0,n1,n2,n3,n0,n2,s31,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u120(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u120)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x38(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x38=0.0d0
      call sum346125(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x38,u120,1.000)
       deallocate(u120)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3214(n2,n3,n0,n1,n1,n3,n0,n2,
     & n1,n3,n0,n1,n2,n3,n0,n2,s31,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u118(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u118)
       deallocate(d1)
       deallocate(d2)
       deallocate(s31)
c
       allocate(x39(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x39=0.0d0
      call sum356124(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x39,u118,1.000)
       deallocate(u118)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s32(n0+1:n2,n1+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s32)
       deallocate(d1)
       deallocate(b2)
c
       call sum4231(n0,n1,n1,n3,n2,n3,n0,n2,x9,s32, 1.000)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2431(n0,n2,n1,n3,n2,n3,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,s32,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u121(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u121)
       deallocate(d1)
       deallocate(d2)
c
       call sum346125(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,
     & x38,u121,-1.000)
       deallocate(u121)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2431(n0,n2,n1,n3,n2,n3,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,s32,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u119(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u119)
       deallocate(d1)
       deallocate(d2)
       deallocate(s32)
c
       call sum356124(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x39,u119,-1.000)
       deallocate(u119)
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
       call sum21(n0,n2,n0,n2,x12,q7, 1.000)
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
       call sum21(n2,n3,n2,n3,x13,q8,-1.000)
       deallocate(q8)
c
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n0,n1,n0,n2,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s33(n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s33)
       deallocate(d1)
       deallocate(b2)
c
       call sum3241(n0,n2,n0,n1,n0,n2,n0,n1,x17,s33, 1.000)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder2413(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s33,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u130(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u130)
       deallocate(d1)
       deallocate(d2)
c
      call sum234156(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x28,u130,1.000)
       deallocate(u130)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder2413(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s33,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u122(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u122)
       deallocate(d1)
       deallocate(d2)
c
      call sum235146(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x29,u122,1.000)
       deallocate(u122)
c
       allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       call reorder4213(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n2,n0,n1,n0,n2,n0,n1,s33,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s158(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s158)
       deallocate(d1)
       deallocate(b2)
       deallocate(s33)
c
       call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s158,-1.000)
       deallocate(s158)
c
       allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n0,n1,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s34(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s34)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n2,n3,n2,n3,n0,n1,x18,s34,-1.000)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder3214(n2,n3,n0,n1,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s34,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u131(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u131)
       deallocate(d1)
       deallocate(d2)
c
       call sum345126(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x27,u131,-1.000)
       deallocate(u131)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder3214(n2,n3,n0,n1,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s34,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u123(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u123)
       deallocate(d1)
       deallocate(d2)
       deallocate(s34)
c
       call sum345126(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,
     & x30,u123,-1.000)
       deallocate(u123)
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
       x10=x10+q9
       deallocate(q9)
c
       allocate(d1(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n1,n3,n0,n2,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s35(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s35)
       deallocate(d1)
       deallocate(b2)
c
       call sum4231(n0,n2,n1,n3,n1,n3,n0,n2,x19,s35, 1.000)
c
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder2431(n0,n2,n1,n3,n1,n3,n0,n2,
     & n1,n3,n0,n2,n1,n3,n0,n2,s35,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u132(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u132)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x40(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x40=0.0d0
      call sum246135(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x40,u132,1.000)
       deallocate(u132)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder2431(n0,n2,n1,n3,n1,n3,n0,n2,
     & n1,n3,n0,n2,n1,n3,n0,n2,s35,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u124(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u124)
       deallocate(d1)
       deallocate(d2)
c
       call sum356124(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x31,u124,-1.000)
       deallocate(u124)
c
       allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4231(n0,n2,n1,n3,n1,n3,n0,n2,
     & n0,n2,n1,n3,n1,n3,n0,n2,s35,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s159(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s159)
       deallocate(d1)
       deallocate(b2)
       deallocate(s35)
c
       call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s159,-1.000)
       deallocate(s159)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s36(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s36)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n2,n3,n1,n3,n2,n3,n1,n3,x20,s36,-1.000)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       call reorder2314(n2,n3,n2,n3,n1,n3,n1,n3,
     & n2,n3,n1,n3,n2,n3,n1,n3,s36,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u133(n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k3
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u133)
       deallocate(d1)
       deallocate(d2)
c
      call sum456123(n1,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,x32,u133,1.000)
       deallocate(u133)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       call reorder3214(n2,n3,n2,n3,n1,n3,n1,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,s36,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u125(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k4
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u125)
       deallocate(d1)
       deallocate(d2)
       deallocate(s36)
c
      call sum456123(n2,n3,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x33,u125,1.000)
       deallocate(u125)
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
       x11=x11+q10
       deallocate(q10)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s37(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s37)
       deallocate(d1)
       deallocate(b2)
c
!       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
!       call reorder4213(n0,n2,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n0,n2,n0,n2,n0,n2,s37,d1)
!       allocate(h2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       call reorder56123478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n2,n0,n2,n2,n3,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4c,h2)
!       allocate(z71(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2
!       i2=k1*k1*k3*k3*k4*k4
!       i3=k2*k2
!       call egemm(i1,i2,i3,d1,h2,z71)
!       deallocate(d1)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2;do n=n0+1,n2
             sum=sum
     &     + (s37(l,m,k,n)*t4c(d,c,b,a,n,m,j,i)       !lmkndcbanmji    (+0.500)
     &     - s37(k,m,l,n)*t4c(d,c,b,a,n,m,j,i))/2.0d0 !kmlndcbanmji    (-0.500)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12347856(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z71, 0.500)
!       call sum12347865(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z71,-0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z71(d,c,b,a,j,i,l,k)      ! 12347856 (+0.500)
!     & -z71(d,c,b,a,j,i,k,l))/2.0d0! 12347865 (-0.500)
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
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder2413(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s37,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(u134(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,d1,d2,u134)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder412563(n2,n3,n1,n3,n0,n1,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,u134,f1)
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z352(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,f1,d2,z352)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - u134(c,a,j,n,l,k)*t2b(d,b,n,i)         !cajnlkdbni      (-1.000)
     &     - u134(d,a,i,n,l,k)*t2b(c,b,n,j)         !dainlkcbnj      (-1.000)
     &     + u134(d,a,j,n,l,k)*t2b(c,b,n,i)         !dajnlkcbni      (+1.000)
     &     + u134(c,a,i,n,l,k)*t2b(d,b,n,j)         !cainlkdbnj      (+1.000)
     &     + u134(c,a,j,n,k,l)*t2b(d,b,n,i)         !cajnkldbni      (+1.000)
     &     + u134(d,a,i,n,k,l)*t2b(c,b,n,j)         !dainklcbnj      (+1.000)
     &     - u134(d,a,j,n,k,l)*t2b(c,b,n,i)         !dajnklcbni      (-1.000)
     &     - u134(c,a,i,n,k,l)*t2b(d,b,n,j)         !cainkldbnj      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13824567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z352,-1.000)
!       call sum23714568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z352,-1.000)
!       call sum23814567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z352, 1.000)
!       call sum13724568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z352, 1.000)
!       call sum13824657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z352, 1.000)
!       call sum23714658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z352, 1.000)
!       call sum23814657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z352,-1.000)
!       call sum13724658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z352,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z352(d,b,i,c,a,l,k,j)       ! 13824567 (-1.000)
!     & -z352(c,b,j,d,a,l,k,i)       ! 23714568 (-1.000)
!     & +z352(c,b,i,d,a,l,k,j)       ! 23814567 (+1.000)
!     & +z352(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!     & +z352(d,b,i,c,a,k,l,j)       ! 13824657 (+1.000)
!     & +z352(c,b,j,d,a,k,l,i)       ! 23714658 (+1.000)
!     & -z352(c,b,i,d,a,k,l,j)       ! 23814657 (-1.000)
!     & -z352(d,b,j,c,a,k,l,i)       ! 13724658 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z352)
       deallocate(u134)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder2413(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s37,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s160(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s160)
       deallocate(d1)
       deallocate(b2)
       deallocate(s37)
c
       call sum2134(n0,n2,n2,n3,n0,n2,n0,n2,x36,s160,-1.000)
       deallocate(s160)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s38(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s38)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x22(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       x22=0.0d0
       call sum3241(n0,n2,n2,n3,n2,n3,n0,n2,x22,s38,-1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,s38,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u135(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u135)
       deallocate(d1)
       deallocate(d2)
c
       call sum346125(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & x28,u135,-1.000)
       deallocate(u135)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,s38,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s161(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s161)
       deallocate(d1)
       deallocate(b2)
       deallocate(s38)
c
       call sum2134(n2,n3,n2,n3,n2,n3,n0,n2,x4,s161, 1.000)
       deallocate(s161)
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
       call sum21(n0,n2,n0,n2,x12,q11,-1.000)
       deallocate(q11)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s39(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s39)
       deallocate(d1)
       deallocate(b2)
c
       call sum4231(n0,n2,n2,n3,n2,n3,n0,n2,x22,s39, 1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder2431(n0,n2,n2,n3,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,s39,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u136(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u136)
       deallocate(d1)
       deallocate(d2)
c
       call sum346125(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & x40,u136,-1.000)
       deallocate(u136)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder4231(n0,n2,n2,n3,n2,n3,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,s39,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s163(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s163)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n2,n3,n2,n3,n2,n3,n0,n2,x37,s163, 1.000)
       deallocate(s163)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder2431(n0,n2,n2,n3,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,s39,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s162(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s162)
       deallocate(d1)
       deallocate(b2)
       deallocate(s39)
c
       call sum3124(n0,n2,n2,n3,n0,n2,n0,n2,x3,s162, 1.000)
       deallocate(s162)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s40(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s40)
       deallocate(d1)
       deallocate(b2)
c
!       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
!       call reorder2341(n2,n3,n2,n3,n2,n3,n2,n3,
!     & n2,n3,n2,n3,n2,n3,n2,n3,s40,d1)
!       allocate(h2(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       call reorder12345678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,t4c,h2)
!       allocate(z75(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1,
!     & n2+1:n3,n2+1:n3))
!       i1=k4*k4
!       i2=k1*k1*k2*k2*k3*k3
!       i3=k4*k4
!       call egemm(i1,i2,i3,d1,h2,z75)
!       deallocate(d1)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do f=n2+1,n3
             sum=sum
     &     + (s40(c,f,e,d)*t4c(f,e,b,a,l,k,j,i)       !cfedfebalkji    (+0.500)
     &     - s40(d,f,e,c)*t4c(f,e,b,a,l,k,j,i))/2.0d0 !dfecfebalkji    (-0.500)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34567812(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z75, 0.500)
!       call sum34567821(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z75,-0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z75(b,a,l,k,j,i,d,c)      ! 34567812 (+0.500)
!     & -z75(b,a,l,k,j,i,c,d))/2.0d0! 34567821 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z75)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder3241(n2,n3,n2,n3,n2,n3,n2,n3,
     & n2,n3,n2,n3,n2,n3,n2,n3,s40,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u137(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4*k4
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u137)
       deallocate(d1)
       deallocate(d2)
       deallocate(s40)
c
!       allocate(f1(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder456123(n1,n3,n0,n2,n0,n1,n2,n3,n2,n3,n2,n3,
!     & n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,u137,f1)
!       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
!       allocate(z355(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k4*k4
!       i2=k1*k2*k3
!       i3=k4
!       call egemm(i1,i2,i3,f1,d2,z355)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum
     &     + u137(a,l,j,f,d,c)*t2b(f,b,k,i)         !aljfdcfbki      (+1.000)
     &     + u137(a,k,i,f,d,c)*t2b(f,b,l,j)         !akifdcfblj      (+1.000)
     &     - u137(a,l,j,f,c,d)*t2b(f,b,k,i)         !aljfcdfbki      (-1.000)
     &     - u137(a,k,i,f,c,d)*t2b(f,b,l,j)         !akifcdfblj      (-1.000)
     &     - u137(a,k,j,f,d,c)*t2b(f,b,l,i)         !akjfdcfbli      (-1.000)
     &     - u137(a,l,i,f,d,c)*t2b(f,b,k,j)         !alifdcfbkj      (-1.000)
     &     + u137(a,k,j,f,c,d)*t2b(f,b,l,i)         !akjfcdfbli      (+1.000)
     &     + u137(a,l,i,f,c,d)*t2b(f,b,k,j)         !alifcdfbkj      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum36812457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z355, 1.000)
!       call sum35712468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z355, 1.000)
!       call sum36821457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z355,-1.000)
!       call sum35721468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z355,-1.000)
!       call sum35812467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z355,-1.000)
!       call sum36712458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z355,-1.000)
!       call sum35821467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z355, 1.000)
!       call sum36721458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z355, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z355(b,k,i,d,c,a,l,j)       ! 36812457 (+1.000)
!     & +z355(b,l,j,d,c,a,k,i)       ! 35712468 (+1.000)
!     & -z355(b,k,i,c,d,a,l,j)       ! 36821457 (-1.000)
!     & -z355(b,l,j,c,d,a,k,i)       ! 35721468 (-1.000)
!     & -z355(b,l,i,d,c,a,k,j)       ! 35812467 (-1.000)
!     & -z355(b,k,j,d,c,a,l,i)       ! 36712458 (-1.000)
!     & +z355(b,l,i,c,d,a,k,j)       ! 35821467 (+1.000)
!     & +z355(b,k,j,c,d,a,l,i)       ! 36721458 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z355)
       deallocate(u137)
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
       x13=x13-q12
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
       call sum356124(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x39,u1,-1.000)
       deallocate(u1)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u2(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u2)
       deallocate(d1)
       deallocate(d2)
c
       call sum346125(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x38,u2,-1.000)
       deallocate(u2)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n0,n2,n0,n1,intm,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u3(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u3)
       deallocate(d1)
       deallocate(d2)
c
       call sum235146(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x29,u3, 1.000)
       deallocate(u3)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n2,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u4(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u4)
       deallocate(d1)
       deallocate(d2)
c
       call sum345126(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x30,u4, 1.000)
       deallocate(u4)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n1,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u5(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u5)
       deallocate(d1)
       deallocate(d2)
c
       call sum356124(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x31,u5,-1.000)
       deallocate(u5)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,intm,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u6(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k4
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u6)
       deallocate(d1)
       deallocate(d2)
c
       call sum456123(n2,n3,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x33,u6,-1.000)
       deallocate(u6)
c
       allocate(b1(n1+1:n3,n0+1:n1))
       call reorder12(0,n3,0,n3,
     & n1,n3,n0,n1,fockr,b1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s41(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,b1,d2,s41)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x5,s41, 1.000)
       deallocate(s41)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n1,n1,n3,fockr,b1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s42(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s42)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x6,s42,-1.000)
       deallocate(s42)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder361245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
       allocate(u7(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k2*k4*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u7)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x41(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x41=0.0d0
       call sum234516(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x41,u7,1.000)
       deallocate(u7)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s43(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s43)
       deallocate(d1)
       deallocate(d2)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x25,s43, 1.000)
       deallocate(s43)
c
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder4312(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(s44(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k3*k3
       i3=k1*k1
       call egemm(i1,i2,i3,d1,d2,s44)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n1,n3,n1,n3,n1,n3,n0,n1,x6,s44, 0.500)
       deallocate(s44)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u8(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u8)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x42(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       x42=0.0d0
       call sum456231(n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,x42,u8,1.000)
       deallocate(u8)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s45(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k1
       i3=k3*k3
       call egemm(i1,i2,i3,d1,d2,s45)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n0,n1,n0,n1,x5,s45, 0.500)
       deallocate(s45)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1324(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,t2a,d2)
       allocate(s46(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k1*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s46)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n1,n3,n1,n3,n1,n3,n0,n1,x26,s46,-1.000)
       deallocate(s46)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n2,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u9(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2*k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u9)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x43(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x43=0.0d0
       call sum356241(n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,x43,u9,1.000)
       deallocate(u9)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n2,n3,n0,n1,intm,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u10(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k4
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u10)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x44(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       x44=0.0d0
       call sum456231(n0,n1,n2,n3,n2,n3,n1,n3,n0,n1,n0,n1,x44,u10,1.000)
       deallocate(u10)
c
       allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n0,n1,n0,n2,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder341256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n1,n3,n0,n2,n2,n3,n2,n3,n0,n2,n0,n1,t3c,f2)
       allocate(u11(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k2*k4*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,f2,u11)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x45(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x45=0.0d0
       call sum234615(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x45,u11,1.000)
       deallocate(u11)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s47(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s47)
       deallocate(d1)
       deallocate(d2)
c
       call sum2431(n0,n2,n1,n3,n0,n2,n0,n1,x7,s47, 1.000)
       deallocate(s47)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder132456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n1,n3,n2,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
       allocate(u12(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4
       i2=k1*k2*k2*k4
       i3=k3*k4
       call egemm(i1,i2,i3,d1,f2,u12)
       deallocate(d1)
       deallocate(f2)
c
       call sum345621(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,
     & x30,u12,1.000)
       deallocate(u12)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n2,n3,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1324(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,t2a,d2)
       allocate(s48(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k1*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s48)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n2,n3,n2,n3,n1,n3,n0,n1,x8,s48, 1.000)
       deallocate(s48)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder142356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
       allocate(u13(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k2*k4*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u13)
       deallocate(d1)
       deallocate(f2)
c
       call sum234516(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,
     & x41,u13,1.000)
       deallocate(u13)
c
       allocate(d1(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u14(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k4
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u14)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x46(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       x46=0.0d0
       call sum456231(n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,
     & x46,u14,1.000)
       deallocate(u14)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u15(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u15)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder546123(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n0,n1,n1,n3,n0,n1,n0,n1,u15,f1)
!       allocate(h2(n0+1:n1,n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2))
!       call reorder83712456(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,t4c,h2)
!       allocate(z99(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k2*k2*k3*k4*k4
!       i3=k1*k3*k1
!       call egemm(i1,i2,i3,f1,h2,z99)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do m=n0+1,n1;do n=n0+1,n1
             sum=sum
     &     + (u15(b,j,i,f,m,n)*t4c(d,c,f,a,l,k,n,m)       !bjifmndcfalknm  (+0.500)
     &     - u15(a,j,i,f,m,n)*t4c(d,c,f,b,l,k,n,m))/2.0d0 !ajifmndcfblknm  (-0.500)
             enddo;enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12456378(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z99, 0.500)
!       call sum12356478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z99,-0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z99(d,c,a,l,k,b,j,i)      ! 12456378 (+0.500)
!     & -z99(d,c,b,l,k,a,j,i))/2.0d0! 12356478 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z99)
c
       allocate(f1(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder465123(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,u15,f1)
       allocate(d2(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,t2b,d2)
       allocate(u152(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k1
       i2=k2*k4
       i3=k1*k3
       call egemm(i1,i2,i3,f1,d2,u152)
       deallocate(f1)
       deallocate(d2)
c
       call sum241356(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x39,u152,-1.000)
       deallocate(u152)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder541236(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u15,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s139(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1*k3
       i3=k3*k1
       call egemm1(i1,i3,f1,b2,s139)
       deallocate(f1)
       deallocate(b2)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x5,s139,-1.000)
       deallocate(s139)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder541236(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u15,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u89(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u89)
       deallocate(f1)
       deallocate(b2)
       deallocate(u15)
c
      call sum324561(n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,x42,u89,-1.000)
       deallocate(u89)
c
!       allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder631245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
!       allocate(z88(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3
!       i2=k2*k2*k4*k4
!       i3=k3*k1
!       call egemm(i1,i2,i3,x42,f2,z88)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + x42(m,f,b,a,j,i)*t3c(d,c,f,l,k,m)      !mfbajidcflkm    (+1.000)
     &     - x42(m,f,a,b,j,i)*t3c(d,c,f,l,k,m)      !mfabjidcflkm    (-1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12563478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z88, 1.000)
!       call sum12564378(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z88,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z88(d,c,l,k,b,a,j,i)       ! 12563478 (+1.000)
!     & -z88(d,c,l,k,a,b,j,i)       ! 12564378 (-1.000)
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
       deallocate(x42)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s49(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1
       i3=k3*k3
       call egemm(i1,i2,i3,d1,d2,s49)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x14(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       x14=0.0d0
       call sum3421(n0,n1,n0,n1,n0,n1,n0,n1,x14,s49, 0.500)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s49,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u153(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u153)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x47(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x47=0.0d0
      call sum234156(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x47,u153,1.000)
       deallocate(u153)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s49,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s138(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s138)
       deallocate(d1)
       deallocate(b2)
       deallocate(s49)
c
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x5,s138,-0.500)
       deallocate(s138)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(h2(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,
     & n0+1:n2,n0+1:n1))
       call reorder34712568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,
     & n1,n0,n1,n1,n3,n1,n3,n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,t4c,h2)
       allocate(u16(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k2*k2*k4*k4
       i3=k1*k3*k3
       call egemm(i1,i2,i3,d1,h2,u16)
       deallocate(d1)
       deallocate(h2)
c
       call sum234561(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x41,u16,0.500)
       deallocate(u16)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1324(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,t2a,d2)
       allocate(s50(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s50)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n1,n3,n0,n1,x15,s50, 1.000)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s50,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u155(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u155)
       deallocate(d1)
       deallocate(d2)
c
       call sum245136(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x27,u155,-1.000)
       deallocate(u155)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n1,n3,n0,n1,n1,n3,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s50,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s140(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s140)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n1,n3,n1,n3,n1,n3,n0,n1,x26,s140,-1.000)
       deallocate(s140)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s50,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s136(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s136)
       deallocate(d1)
       deallocate(b2)
       deallocate(s50)
c
       call sum4123(n0,n1,n1,n3,n0,n1,n0,n1,x25,s136,-1.000)
       deallocate(s136)
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
       call sum21(n0,n1,n0,n1,x10,q13,-0.500)
       deallocate(q13)
c
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder4312(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(s51(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k3*k3
       i3=k1*k1
       call egemm(i1,i2,i3,d1,d2,s51)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x16(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       x16=0.0d0
       call sum3412(n1,n3,n1,n3,n1,n3,n1,n3,x16,s51, 0.500)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder4312(n1,n3,n1,n3,n1,n3,n1,n3,
     & n1,n3,n1,n3,n1,n3,n1,n3,s51,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u156(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u156)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x48(n1+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       x48=0.0d0
      call sum256134(n1,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,x48,u156,1.000)
       deallocate(u156)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder4312(n1,n3,n1,n3,n1,n3,n1,n3,
     & n1,n3,n1,n3,n1,n3,n1,n3,s51,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s137(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s137)
       deallocate(d1)
       deallocate(b2)
       deallocate(s51)
c
       call sum4123(n1,n3,n1,n3,n1,n3,n0,n1,x6,s137, 0.500)
       deallocate(s137)
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
       call sum21(n1,n3,n1,n3,x11,q14, 0.500)
       deallocate(q14)
c
       allocate(d1(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u17(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u17)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder546123(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
!     & n0,n1,n2,n3,n0,n2,n1,n3,n0,n1,n0,n1,u17,f1)
!       allocate(h2(n0+1:n1,n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2))
!       call reorder81523467(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n1,n2,n3,n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,t4d,h2)
!       allocate(z106(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k2*k2*k3*k4*k4
!       i3=k2*k4*k1
!       call egemm(i1,i2,i3,f1,h2,z106)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     + u17(b,j,i,f,m,n)*t4d(f,d,c,a,n,l,k,m)  !bjifmnfdcanlkm  (+1.000)
     &     - u17(a,j,i,f,m,n)*t4d(f,d,c,b,n,l,k,m)  !ajifmnfdcbnlkm  (-1.000)
             enddo;enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12456378(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z106, 1.000)
!       call sum12356478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z106,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z106(d,c,a,l,k,b,j,i)       ! 12456378 (+1.000)
!     & -z106(d,c,b,l,k,a,j,i)       ! 12356478 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z106)
c
c
       allocate(f1(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder564123(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n0,n2,n2,n3,n1,n3,n0,n1,n0,n1,u17,f1)
       allocate(d2(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder4312(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n0,n2,n2,n3,n1,n3,t2b,d2)
       allocate(u159(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k4
       i2=k3*k4
       i3=k2*k1
       call egemm(i1,i2,i3,f1,d2,u159)
       deallocate(f1)
       deallocate(d2)
c
      call sum241356(n2,n3,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x33,u159,1.000)
       deallocate(u159)
c
       allocate(f1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n2,n3,n1,n3,n0,n1,n0,n1,n0,n2,u17,f1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u158(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1*k3
       i2=k2*k3
       i3=k4*k1
       call egemm(i1,i2,i3,f1,d2,u158)
       deallocate(f1)
       deallocate(d2)
c
       call sum342561(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x31,u158,-1.000)
       deallocate(u158)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder465123(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,u17,f1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(u157(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k1
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,f1,d2,u157)
       deallocate(f1)
       deallocate(d2)
c
       call sum241356(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x39,u157,-1.000)
       deallocate(u157)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder465123(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,u17,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s170(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k1
       i3=k2*k4
       call egemm1(i1,i3,f1,b2,s170)
       deallocate(f1)
       deallocate(b2)
c
       x5=x5+s170
       deallocate(s170)
c
       allocate(f1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder654123(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n2,n0,n1,n2,n3,n1,n3,n0,n1,n0,n1,u17,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u127(n2+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u127)
       deallocate(f1)
       deallocate(b2)
c
       call sum312456(n0,n1,n2,n3,n2,n3,n1,n3,n0,n1,n0,n1,
     & x44,u127,-1.000)
       deallocate(u127)
c
       allocate(f1(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder451236(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n1,n1,n3,n0,n1,n0,n1,n0,n2,u17,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u126(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1*k3*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u126)
       deallocate(f1)
       deallocate(b2)
c
      call sum423561(n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,x43,u126,1.000)
c
       allocate(f1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder623145(n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,n0,n2,
     & n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,u126,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u189(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u189)
       deallocate(f1)
       deallocate(b2)
       deallocate(u126)
c
      call sum213456(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x39,u189,1.000)
       deallocate(u189)
c
       allocate(f1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n2,n3,n1,n3,n0,n1,n0,n1,n0,n2,u17,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u91(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1*k3*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u91)
       deallocate(f1)
       deallocate(b2)
       deallocate(u17)
c
      call sum324561(n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x46,u91,-1.000)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder412356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
!       allocate(z98(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3
!       i2=k2*k2*k4*k4
!       i3=k4*k2
!       call egemm(i1,i2,i3,x46,f2,z98)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x46(m,f,b,a,j,i)*t3d(f,d,c,m,l,k)      !mfbajifdcmlk    (+1.000)
     &     - x46(m,f,a,b,j,i)*t3d(f,d,c,m,l,k)      !mfabjifdcmlk    (-1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12563478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z98, 1.000)
!       call sum12564378(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z98,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z98(d,c,l,k,b,a,j,i)       ! 12563478 (+1.000)
!     & -z98(d,c,l,k,a,b,j,i)       ! 12564378 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z98)
       deallocate(x46)
c
       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder621345(n1,n3,n2,n3,n1,n3,n0,n1,n0,n1,n0,n2,
     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,u91,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u184(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u184)
       deallocate(f1)
       deallocate(b2)
c
       call sum213456(n2,n3,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,
     & x33,u184,-1.000)
       deallocate(u184)
c
!       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
!       allocate(z267(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3*k4
!       i2=k2*k2*k4
!       i3=k4
!       call egemm(i1,i2,i3,x33,d2,z267)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum
     &     - x33(f,d,b,a,j,i)*t2c(f,c,l,k)          !fdbajifclk      (-1.000)
     &     + x33(f,d,a,b,j,i)*t2c(f,c,l,k)          !fdabjifclk      (+1.000)
     &     + x33(f,c,b,a,j,i)*t2c(f,d,l,k)          !fcbajifdlk      (+1.000)
     &     - x33(f,c,a,b,j,i)*t2c(f,d,l,k)          !fcabjifdlk      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum25613478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z267,-1.000)
!       call sum25614378(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z267, 1.000)
!       call sum15623478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z267, 1.000)
!       call sum15624378(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z267,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z267(c,l,k,d,b,a,j,i)       ! 25613478 (-1.000)
!     & +z267(c,l,k,d,a,b,j,i)       ! 25614378 (+1.000)
!     & +z267(d,l,k,c,b,a,j,i)       ! 15623478 (+1.000)
!     & -z267(d,l,k,c,a,b,j,i)       ! 15624378 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z267)
       deallocate(x33)
c
       allocate(f1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder261345(n1,n3,n2,n3,n1,n3,n0,n1,n0,n1,n0,n2,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,u91,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u183(n0+1:n2,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u183)
       deallocate(f1)
       deallocate(b2)
       deallocate(u91)
c
      call sum412356(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x31,u183,1.000)
       deallocate(u183)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1)
       allocate(h2(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,
     & n0+1:n2,n0+1:n1))
       call reorder14523678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
     & n0,n1,n2,n3,n1,n3,n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,t4d,h2)
       allocate(u18(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k2*k2*k4*k4
       i3=k2*k3*k4
       call egemm(i1,i2,i3,d1,h2,u18)
       deallocate(d1)
       deallocate(h2)
c
       call sum234561(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x41,u18,1.000)
       deallocate(u18)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1324(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,t2a,d2)
       allocate(s52(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s52)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n1,n3,n0,n1,x24,s52, 1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s52,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u164(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u164)
       deallocate(d1)
       deallocate(d2)
c
       call sum245136(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & x34,u164,-1.000)
       deallocate(u164)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s52,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u162(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u162)
       deallocate(d1)
       deallocate(d2)
c
       call sum345126(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x35,u162,-1.000)
       deallocate(u162)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n1,n3,n0,n1,n2,n3,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n1,s52,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s172(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s172)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x8,s172,-1.000)
       deallocate(s172)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s52,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s171(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s171)
       deallocate(d1)
       deallocate(b2)
       deallocate(s52)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x7,s171, 1.000)
       deallocate(s171)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u19(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u19)
       deallocate(d1)
       deallocate(d2)
c
      call sum234156(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x47,u19, 2.000)
       deallocate(u19)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u20(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u20)
       deallocate(d1)
       deallocate(d2)
c
      call sum245136(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x27,u20,-1.000)
       deallocate(u20)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u21(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u21)
       deallocate(d1)
       deallocate(d2)
c
      call sum256134(n1,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,x48,u21, 2.000)
       deallocate(u21)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n0,n2,n0,n1,intm,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u22(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u22)
       deallocate(d1)
       deallocate(d2)
c
      call sum234156(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x28,u22, 1.000)
       deallocate(u22)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n2,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u23(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u23)
       deallocate(d1)
       deallocate(d2)
c
      call sum345126(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x27,u23,1.000)
       deallocate(u23)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n1,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u24(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u24)
       deallocate(d1)
       deallocate(d2)
c
      call sum246135(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x40,u24, 1.000)
       deallocate(u24)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n1,n3,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u25(n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k3
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u25)
       deallocate(d1)
       deallocate(d2)
c
      call sum456123(n1,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,x32,u25,-1.000)
       deallocate(u25)
c
!       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
!       allocate(z285(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,
!     & n1+1:n3,n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k3*k4
!       i2=k1*k2*k4
!       i3=k3
!       call egemm(i1,i2,i3,x32,d2,z285)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + x32(e,d,b,a,l,j)*t2b(c,e,k,i)          !edbaljceki      (+1.000)
     &     - x32(e,d,a,b,l,j)*t2b(c,e,k,i)          !edabljceki      (-1.000)
     &     + x32(e,d,b,a,k,i)*t2b(c,e,l,j)          !edbakicelj      (+1.000)
     &     - x32(e,d,a,b,k,i)*t2b(c,e,l,j)          !edabkicelj      (-1.000)
     &     - x32(e,c,b,a,l,j)*t2b(d,e,k,i)          !ecbaljdeki      (-1.000)
     &     + x32(e,c,a,b,l,j)*t2b(d,e,k,i)          !ecabljdeki      (+1.000)
     &     - x32(e,c,b,a,k,i)*t2b(d,e,l,j)          !ecbakidelj      (-1.000)
     &     + x32(e,c,a,b,k,i)*t2b(d,e,l,j)          !ecabkidelj      (+1.000)
     &     - x32(e,d,b,a,k,j)*t2b(c,e,l,i)          !edbakjceli      (-1.000)
     &     + x32(e,d,a,b,k,j)*t2b(c,e,l,i)          !edabkjceli      (+1.000)
     &     - x32(e,d,b,a,l,i)*t2b(c,e,k,j)          !edbalicekj      (-1.000)
     &     + x32(e,d,a,b,l,i)*t2b(c,e,k,j)          !edablicekj      (+1.000)
     &     + x32(e,c,b,a,k,j)*t2b(d,e,l,i)          !ecbakjdeli      (+1.000)
     &     - x32(e,c,a,b,k,j)*t2b(d,e,l,i)          !ecabkjdeli      (-1.000)
     &     + x32(e,c,b,a,l,i)*t2b(d,e,k,j)          !ecbalidekj      (+1.000)
     &     - x32(e,c,a,b,l,i)*t2b(d,e,k,j)          !ecablidekj      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum26813457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285, 1.000)
!       call sum26814357(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285,-1.000)
!       call sum25713468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285, 1.000)
!       call sum25714368(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285,-1.000)
!       call sum16823457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285,-1.000)
!       call sum16824357(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285, 1.000)
!       call sum15723468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285,-1.000)
!       call sum15724368(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285, 1.000)
!       call sum25813467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285,-1.000)
!       call sum25814367(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285, 1.000)
!       call sum26713458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285,-1.000)
!       call sum26714358(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285, 1.000)
!       call sum15823467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285, 1.000)
!       call sum15824367(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285,-1.000)
!       call sum16723458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285, 1.000)
!       call sum16724358(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z285,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z285(c,k,i,d,b,a,l,j)       ! 26813457 (+1.000)
!     & -z285(c,k,i,d,a,b,l,j)       ! 26814357 (-1.000)
!     & +z285(c,l,j,d,b,a,k,i)       ! 25713468 (+1.000)
!     & -z285(c,l,j,d,a,b,k,i)       ! 25714368 (-1.000)
!     & -z285(d,k,i,c,b,a,l,j)       ! 16823457 (-1.000)
!     & +z285(d,k,i,c,a,b,l,j)       ! 16824357 (+1.000)
!     & -z285(d,l,j,c,b,a,k,i)       ! 15723468 (-1.000)
!     & +z285(d,l,j,c,a,b,k,i)       ! 15724368 (+1.000)
!     & -z285(c,l,i,d,b,a,k,j)       ! 25813467 (-1.000)
!     & +z285(c,l,i,d,a,b,k,j)       ! 25814367 (+1.000)
!     & -z285(c,k,j,d,b,a,l,i)       ! 26713458 (-1.000)
!     & +z285(c,k,j,d,a,b,l,i)       ! 26714358 (+1.000)
!     & +z285(d,l,i,c,b,a,k,j)       ! 15823467 (+1.000)
!     & -z285(d,l,i,c,a,b,k,j)       ! 15824367 (-1.000)
!     & +z285(d,k,j,c,b,a,l,i)       ! 16723458 (+1.000)
!     & -z285(d,k,j,c,a,b,l,i)       ! 16724358 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z285)
       deallocate(x32)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(u26(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,d1,d2,u26)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x49(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x49=0.0d0
      call sum236145(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x49,u26, 1.000)
       deallocate(u26)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u27(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u27)
       deallocate(d1)
       deallocate(d2)
c
      call sum346125(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x40,u27,-1.000)
       deallocate(u27)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n2,n3,intb,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u28(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4*k4
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u28)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x50(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       x50=0.0d0
      call sum456123(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x50,u28, 1.000)
       deallocate(u28)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u29(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u29)
       deallocate(d1)
       deallocate(d2)
c
      call sum345126(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x35,u29,-1.000)
       deallocate(u29)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u30(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u30)
       deallocate(d1)
       deallocate(d2)
c
      call sum245136(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x34,u30,-1.000)
       deallocate(u30)
c
       allocate(b1(n1+1:n3,n0+1:n1))
       call reorder12(0,n3,0,n3,
     & n1,n3,n0,n1,fockr,b1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s53(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,b1,d2,s53)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s53, 1.000)
       deallocate(s53)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n1,n1,n3,fockr,b1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(s54(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
       i1=k3
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s54)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x2,s54,-1.000)
       deallocate(s54)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u31(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u31)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x51(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x51=0.0d0
      call sum345261(n0,n1,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,x51,u31, 1.000)
       deallocate(u31)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder251346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(u32(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k2*k3*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u32)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x52(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x52=0.0d0
      call sum234516(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x52,u32, 1.000)
       deallocate(u32)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(s55(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s55)
       deallocate(d1)
       deallocate(d2)
c
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s55,-1.000)
       deallocate(s55)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u33(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u33)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x53(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       x53=0.0d0
      call sum356241(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,x53,u33, 1.000)
       deallocate(u33)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder231456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n1,n3,n2,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
       allocate(u34(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k1*k2*k4
       i3=k3*k3
       call egemm(i1,i2,i3,d1,f2,u34)
       deallocate(d1)
       deallocate(f2)
c
      call sum245631(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x39,u34,-0.500)
       deallocate(u34)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,t2b,d2)
       allocate(s56(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k2*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s56)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n1,n3,n2,n3,n1,n3,n0,n2,x2,s56,-1.000)
       deallocate(s56)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n2,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u35(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u35)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x54(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x54=0.0d0
      call sum346251(n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,x54,u35, 1.000)
       deallocate(u35)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder521346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(u36(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k2*k3*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,f2,u36)
       deallocate(d1)
       deallocate(f2)
c
      call sum234651(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x28,u36,-1.000)
       deallocate(u36)
c
       allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n0,n1,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder2314(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n2,n2,n3,n0,n1,t2b,d2)
       allocate(s57(n2+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,d2,s57)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n0,n1,n2,n3,n0,n2,n0,n1,x1,s57,-1.000)
       deallocate(s57)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n2,n3,n0,n1,intm,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u37(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k4
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u37)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x55(n0+1:n1,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       x55=0.0d0
      call sum456231(n0,n1,n2,n3,n2,n3,n2,n3,n0,n2,n0,n1,x55,u37, 1.000)
       deallocate(u37)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u38(n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k3
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u38)
       deallocate(d1)
       deallocate(d2)
c
      call sum456231(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,x53,u38, 1.000)
       deallocate(u38)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s58(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4
       i2=k1*k2
       i3=k3*k4
       call egemm(i1,i2,i3,d1,d2,s58)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n2,n3,n0,n2,n0,n1,x1,s58, 1.000)
       deallocate(s58)
c
       allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n0,n1,n0,n2,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder241356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n2,n2,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(u39(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k1*k3*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,f2,u39)
       deallocate(d1)
       deallocate(f2)
c
      call sum235614(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x39,u39, 1.000)
       deallocate(u39)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(s59(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s59)
       deallocate(d1)
       deallocate(d2)
c
       call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x36,s59,-1.000)
       deallocate(s59)
c
       allocate(d1(n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder1234(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n1,n3,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder4312(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n0,n2,n2,n3,n1,n3,t2b,d2)
       allocate(s60(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k3*k4
       i3=k2*k1
       call egemm(i1,i2,i3,d1,d2,s60)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n1,n3,n2,n3,n1,n3,n0,n2,x2,s60, 1.000)
       deallocate(s60)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder123456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
       allocate(u40(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4
       i2=k1*k1*k2*k3
       i3=k3*k4
       call egemm(i1,i2,i3,d1,f2,u40)
       deallocate(d1)
       deallocate(f2)
c
      call sum345621(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x39,u40,-1.000)
       deallocate(u40)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n2,n3,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,t2b,d2)
       allocate(s61(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k2*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s61)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n2,n3,n2,n3,n2,n3,n0,n2,x37,s61, 1.000)
       deallocate(s61)
c
       allocate(d1(n2+1:n3,n0+1:n1,n1+1:n3,n2+1:n3))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n1,n3,n2,n3,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder1423(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n1,n1,n3,n0,n2,t2b,d2)
       allocate(s62(n1+1:n3,n0+1:n2,n1+1:n3,n2+1:n3))
       i1=k4*k3
       i2=k2*k3
       i3=k1*k4
       call egemm(i1,i2,i3,d1,d2,s62)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n1,n3,n2,n3,n1,n3,n0,n2,x2,s62,-1.000)
       deallocate(s62)
c
       allocate(b1(n2+1:n3,n0+1:n2))
       call reorder12(0,n3,0,n3,
     & n2,n3,n0,n2,fockb,b1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s63(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,b1,d2,s63)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x7,s63, 1.000)
       deallocate(s63)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n2,n2,n3,fockb,b1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s64(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,b1,d2,s64)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n2,n3,n2,n3,n1,n3,n0,n1,x8,s64,-1.000)
       deallocate(s64)
c
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n0,n1,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u41(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u41)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x56(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x56=0.0d0
      call sum345261(n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,x56,u41, 1.000)
       deallocate(u41)
c
c
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(f2(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder612345(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,t3c,f2)
       allocate(u42(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k2*k2*k3*k4
       i3=k4*k1
       call egemm(i1,i2,i3,d1,f2,u42)
       deallocate(d1)
       deallocate(f2)
c
      call sum234561(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x34,u42, 1.000)
       deallocate(u42)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s65(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s65)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n0,n1,n1,n3,n0,n1,n0,n1,x25,s65,-1.000)
       deallocate(s65)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder142356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
       allocate(u43(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k2*k3*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u43)
       deallocate(d1)
       deallocate(f2)
c
      call sum234516(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x52,u43, 1.000)
       deallocate(u43)
c
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(s66(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k2*k3
       i3=k4*k1
       call egemm(i1,i2,i3,d1,d2,s66)
       deallocate(d1)
       deallocate(d2)
c
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x7,s66,-1.000)
       deallocate(s66)
c
       allocate(d1(n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n2,n3,n0,n1,intm,d1)
       allocate(d2(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder4312(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n0,n2,n2,n3,n1,n3,t2b,d2)
       allocate(s67(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4
       i2=k3*k4
       i3=k2*k1
       call egemm(i1,i2,i3,d1,d2,s67)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n2,n3,n2,n3,n1,n3,n0,n1,x8,s67, 1.000)
       deallocate(s67)
c
       allocate(d1(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u44(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k4
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u44)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x57(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       x57=0.0d0
      call sum356241(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x57,u44, 1.000)
       deallocate(u44)
c
       allocate(d1(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u45(n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u45)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x58(n0+1:n2,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       x58=0.0d0
      call sum456231(n0,n2,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,x58,u45, 1.000)
       deallocate(u45)
c
       allocate(d1(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s68(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k1*k2
       i3=k3*k4
       call egemm(i1,i2,i3,d1,d2,s68)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n1,n3,n0,n2,n0,n1,x7,s68, 1.000)
       deallocate(s68)
c
       allocate(d1(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n1,n3,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder132456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n1,n3,n2,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
       allocate(u46(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k1*k2*k2*k4
       i3=k3*k4
       call egemm(i1,i2,i3,d1,f2,u46)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x59(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x59=0.0d0
      call sum245631(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x59,u46, 1.000)
       deallocate(u46)
c
       allocate(d1(n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n2,n3,n1,n3,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder2314(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n2,n2,n3,n0,n1,t2b,d2)
       allocate(s69(n2+1:n3,n0+1:n1,n2+1:n3,n1+1:n3))
       i1=k3*k4
       i2=k1*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,d2,s69)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n2,n3,n2,n3,n1,n3,n0,n1,x8,s69,-1.000)
       deallocate(s69)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n1,n3,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s70(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s70)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n1,n3,n1,n3,n1,n3,n0,n1,x26,s70, 1.000)
       deallocate(s70)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u47(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u47)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x60(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x60=0.0d0
      call sum346251(n0,n2,n0,n2,n1,n3,n0,n2,n0,n2,n0,n1,x60,u47, 1.000)
       deallocate(u47)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder142356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
       allocate(u48(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k2*k3*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u48)
       deallocate(d1)
       deallocate(f2)
c
      call sum234615(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x40,u48, 1.000)
       deallocate(u48)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s71(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s71)
       deallocate(d1)
       deallocate(d2)
c
       call sum2431(n0,n2,n1,n3,n0,n2,n0,n1,x7,s71,-1.000)
       deallocate(s71)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u49(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u49)
       deallocate(d1)
       deallocate(d2)
c
      call sum456231(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x57,u49, 1.000)
       deallocate(u49)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder123456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
       allocate(u50(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k2*k2*k3
       i3=k4*k4
       call egemm(i1,i2,i3,d1,f2,u50)
       deallocate(d1)
       deallocate(f2)
c
      call sum345621(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x59,u50, 0.500)
       deallocate(u50)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s72(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s72)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n2,n3,n2,n3,n1,n3,n0,n1,x8,s72,-1.000)
       deallocate(s72)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u51(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u51)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder546123(n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n0,n1,n2,n3,n0,n2,n0,n1,u51,f1)
!       allocate(h2(n0+1:n1,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,
!     & n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder72613458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t4b,h2)
!       allocate(z161(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k4
!       i2=k1*k2*k3*k3*k4
!       i3=k1*k3*k1
!       call egemm(i1,i2,i3,f1,h2,z161)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do m=n0+1,n1;do n=n0+1,n1
             sum=sum                             !top two switched
     &     + (u51(c,k,i,f,m,n)*t4b(d,f,b,a,l,n,m,j)       !ckifmndfbalnmj  (+0.500)
     &     - u51(d,k,i,f,m,n)*t4b(c,f,b,a,l,n,m,j)        !dkifmncfbalnmj  (-0.500)
     &     + u51(d,l,i,f,m,n)*t4b(c,f,b,a,k,n,m,j)        !dlifmncfbaknmj  (+0.500)
     &     - u51(c,l,i,f,m,n)*t4b(d,f,b,a,k,n,m,j)        !clifmndfbaknmj  (-0.500)
     &     + u51(d,k,j,f,m,n)*t4b(c,f,b,a,l,n,m,i)        !dkjfmncfbalnmi  (+0.500)
     &     - u51(c,k,j,f,m,n)*t4b(d,f,b,a,l,n,m,i)        !ckjfmndfbalnmi  (-0.500)
     &     - u51(d,l,j,f,m,n)*t4b(c,f,b,a,k,n,m,i)        !dljfmncfbaknmi  (-0.500)
     &     + u51(c,l,j,f,m,n)*t4b(d,f,b,a,k,n,m,i))/2.0d0 !cljfmndfbaknmi  (+0.500)

             enddo;enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23457168(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z161,-0.500)
!       call sum13457268(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z161, 0.500)
!       call sum23467158(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z161, 0.500)
!       call sum13467258(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z161,-0.500)
!       call sum23458167(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z161, 0.500)
!       call sum13458267(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z161,-0.500)
!       call sum23468157(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z161,-0.500)
!       call sum13468257(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z161, 0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z161(d,b,a,l,j,c,k,i)      ! 13457268 (+0.500) top two switched
!     & -z161(c,b,a,l,j,d,k,i)       ! 23457168 (-0.500)
!     & +z161(c,b,a,k,j,d,l,i)       ! 23467158 (+0.500)
!     & -z161(d,b,a,k,j,c,l,i)       ! 13467258 (-0.500)
!     & +z161(c,b,a,l,i,d,k,j)       ! 23458167 (+0.500)
!     & -z161(d,b,a,l,i,c,k,j)       ! 13458267 (-0.500)
!     & -z161(c,b,a,k,i,d,l,j)       ! 23468157 (-0.500)
!     & +z161(d,b,a,k,i,c,l,j))/2.0d0! 13468257 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z161)
c
       allocate(f1(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder465123(n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n2,n3,n0,n2,n0,n1,u51,f1)
       allocate(d2(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,t2b,d2)
       allocate(u154(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k4*k1
       i2=k2*k4
       i3=k1*k3
       call egemm(i1,i2,i3,f1,d2,u154)
       deallocate(f1)
       deallocate(d2)
c
       call sum341256(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,
     & x38,u154,-1.000)
       deallocate(u154)
c
       allocate(f1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder541236(n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,n0,n1,n0,n1,u51,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s143(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k4
       i3=k3*k1
       call egemm1(i1,i3,f1,b2,s143)
       deallocate(f1)
       deallocate(b2)
c
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s143,-1.000)
       deallocate(s143)
c
       allocate(f1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder541236(n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,n0,n1,n0,n1,u51,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u106(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k4*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u106)
       deallocate(f1)
       deallocate(b2)
c
       call sum423561(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,
     & x53,u106,-1.000)
c
       allocate(f1(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder623145(n1,n3,n1,n3,n2,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,u106,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u178(n1+1:n3,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k4*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u178)
       deallocate(f1)
       deallocate(b2)
       deallocate(u106)
c
      call sum312456(n1,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,x48,u178,2.000)
       deallocate(u178)
c
!       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
!       allocate(z392(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k3*k4
!       i2=k1*k2*k4
!       i3=k3
!       call egemm(i1,i2,i3,x48,d2,z392)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3
             sum=sum                                !top two switched
     &     + (x48(f,c,b,a,k,i)*t2b(d,f,l,j)         !fcbakidflj      (+0.500)
     &     - x48(f,d,b,a,k,i)*t2b(c,f,l,j)          !fdbakicflj      (-0.500)
     &     + x48(f,d,b,a,l,i)*t2b(c,f,k,j)          !fdbalicfkj      (+0.500)
     &     - x48(f,c,b,a,l,i)*t2b(d,f,k,j))/2.0d0   !fcbalidfkj      (-0.500)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum25713468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z392,-0.500)
!       call sum15723468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z392, 0.500)
!       call sum26713458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z392, 0.500)
!       call sum16723458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z392,-0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)     !top two switched
!     & +(z392(d,l,j,c,b,a,k,i)      ! 15723468 (+0.500)
!     & -z392(c,l,j,d,b,a,k,i)       ! 25713468 (-0.500)
!     & +z392(c,k,j,d,b,a,l,i)       ! 26713458 (+0.500)
!     & -z392(d,k,j,c,b,a,l,i))/2.0d0! 16723458 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z392)
       deallocate(x48)
c
       allocate(f1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder451236(n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,u51,f1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(u104(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k4*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,f1,b2,u104)
       deallocate(f1)
       deallocate(b2)
       deallocate(u51)
c
       call sum623451(n0,n1,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,
     & x51,u104,-1.000)
c
!       allocate(f2(n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
!       call reorder561234(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,t3b,f2)
!       allocate(z123(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k4
!       i2=k2*k3*k3*k4
!       i3=k1*k1
!       call egemm(i1,i2,i3,x51,f2,z123)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum                                  !top two switched
     &     + (x51(n,m,c,k,j,i)*t3b(d,b,a,l,n,m)       !nmckjidbalnm    (+0.500)
     &     - x51(n,m,d,k,j,i)*t3b(c,b,a,l,n,m)        !nmdkjicbalnm    (-0.500)
     &     + x51(n,m,d,l,j,i)*t3b(c,b,a,k,n,m)        !nmdljicbaknm    (+0.500)
     &     - x51(n,m,c,l,j,i)*t3b(d,b,a,k,n,m)        !nmcljidbaknm    (-0.500)
     &     + x51(n,m,d,k,i,j)*t3b(c,b,a,l,n,m)        !nmdkijcbalnm    (+0.500)
     &     - x51(n,m,c,k,i,j)*t3b(d,b,a,l,n,m)        !nmckijdbalnm    (-0.500)
     &     - x51(n,m,d,l,i,j)*t3b(c,b,a,k,n,m)        !nmdlijcbaknm    (-0.500)
     &     + x51(n,m,c,l,i,j)*t3b(d,b,a,k,n,m))/2.0d0 !nmclijdbaknm    (+0.500)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23451678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z123,-0.500)
!       call sum13452678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z123, 0.500)
!       call sum23461578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z123, 0.500)
!       call sum13462578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z123,-0.500)
!       call sum23451687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z123, 0.500)
!       call sum13452687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z123,-0.500)
!       call sum23461587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z123,-0.500)
!       call sum13462587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z123, 0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)    !top two switched
!     & +(z123(d,b,a,l,c,k,j,i)      ! 13452678 (+0.500)
!     & -z123(c,b,a,l,d,k,j,i)       ! 23451678 (-0.500)
!     & +z123(c,b,a,k,d,l,j,i)       ! 23461578 (+0.500)
!     & -z123(d,b,a,k,c,l,j,i)       ! 13462578 (-0.500)
!     & +z123(c,b,a,l,d,k,i,j)       ! 23451687 (+0.500)
!     & -z123(d,b,a,l,c,k,i,j)       ! 13452687 (-0.500)
!     & -z123(c,b,a,k,d,l,i,j)       ! 23461587 (-0.500)
!     & +z123(d,b,a,k,c,l,i,j))/2.0d0! 13462587 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z123)
       deallocate(x51)
c
       allocate(f1(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder263451(n0,n1,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,u104,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u177(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k4*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u177)
       deallocate(f1)
       deallocate(b2)
       deallocate(u104)
c
       call sum312456(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x27,u177,-1.000)
       deallocate(u177)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(h2(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
     & n0+1:n1,n0+1:n1))
       call reorder23614578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,
     & n1,n0,n1,n1,n3,n1,n3,n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4b,h2)
       allocate(u52(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k2*k3*k4
       i3=k1*k3*k3
       call egemm(i1,i2,i3,d1,h2,u52)
       deallocate(d1)
       deallocate(h2)
c
      call sum234561(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x39,u52,-0.500)
       deallocate(u52)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,t2b,d2)
       allocate(s73(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s73)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n2,n3,n0,n2,x9,s73, 1.000)
c
       allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4312(n2,n3,n0,n2,n1,n3,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,s73,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s144(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s144)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n1,n3,n2,n3,n1,n3,n0,n2,x2,s144,-1.000)
       deallocate(s144)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3412(n2,n3,n0,n2,n1,n3,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,s73,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s142(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s142)
       deallocate(d1)
       deallocate(b2)
       deallocate(s73)
c
       call sum4123(n0,n1,n2,n3,n0,n2,n0,n1,x1,s142, 1.000)
       deallocate(s142)
c
       allocate(d1(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u53(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u53)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder546123(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
!     & n0,n1,n2,n3,n0,n2,n2,n3,n0,n2,n0,n1,u53,f1)
!       allocate(h2(n0+1:n1,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       call reorder71523468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n1,n2,n3,n0,n2,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t4c,h2)
!       allocate(z164(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k4
!       i2=k1*k2*k3*k3*k4
!       i3=k2*k4*k1
!       call egemm(i1,i2,i3,f1,h2,z164)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     - u53(d,k,i,f,m,n)*t4c(f,c,b,a,n,l,m,j)  !dkifmnfcbanlmj  (-1.000)
     &     + u53(c,k,i,f,m,n)*t4c(f,d,b,a,n,l,m,j)  !ckifmnfdbanlmj  (+1.000)
     &     + u53(d,l,i,f,m,n)*t4c(f,c,b,a,n,k,m,j)  !dlifmnfcbankmj  (+1.000)
     &     - u53(c,l,i,f,m,n)*t4c(f,d,b,a,n,k,m,j)  !clifmnfdbankmj  (-1.000)
     &     + u53(d,k,j,f,m,n)*t4c(f,c,b,a,n,l,m,i)  !dkjfmnfcbanlmi  (+1.000)
     &     - u53(c,k,j,f,m,n)*t4c(f,d,b,a,n,l,m,i)  !ckjfmnfdbanlmi  (-1.000)
     &     - u53(d,l,j,f,m,n)*t4c(f,c,b,a,n,k,m,i)  !dljfmnfcbankmi  (-1.000)
     &     + u53(c,l,j,f,m,n)*t4c(f,d,b,a,n,k,m,i)  !cljfmnfdbankmi  (+1.000)
             enddo;enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23457168(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z164,-1.000)
!       call sum13457268(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z164, 1.000)
!       call sum23467158(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z164, 1.000)
!       call sum13467258(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z164,-1.000)
!       call sum23458167(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z164, 1.000)
!       call sum13458267(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z164,-1.000)
!       call sum23468157(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z164,-1.000)
!       call sum13468257(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z164, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z164(c,b,a,l,j,d,k,i)       ! 23457168 (-1.000)
!     & +z164(d,b,a,l,j,c,k,i)       ! 13457268 (+1.000)
!     & +z164(c,b,a,k,j,d,l,i)       ! 23467158 (+1.000)
!     & -z164(d,b,a,k,j,c,l,i)       ! 13467258 (-1.000)
!     & +z164(c,b,a,l,i,d,k,j)       ! 23458167 (+1.000)
!     & -z164(d,b,a,l,i,c,k,j)       ! 13458267 (-1.000)
!     & -z164(c,b,a,k,i,d,l,j)       ! 23468157 (-1.000)
!     & +z164(d,b,a,k,i,c,l,j)       ! 13468257 (+1.000)
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
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder465123(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n1,u53,f1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(u167(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k4*k1
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,f1,d2,u167)
       deallocate(f1)
       deallocate(d2)
c
      call sum351246(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x27,u167,1.000)
       deallocate(u167)
c
       allocate(f1(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n2,n3,n2,n3,n0,n2,n0,n1,n0,n2,u53,f1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u166(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k4
       i2=k2*k3
       i3=k4*k1
       call egemm(i1,i2,i3,f1,d2,u166)
       deallocate(f1)
       deallocate(d2)
c
       call sum342561(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & x28,u166,-1.000)
       deallocate(u166)
c
       allocate(f1(n0+1:n1,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder564123(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n0,n2,n2,n3,n2,n3,n0,n2,n0,n1,u53,f1)
       allocate(d2(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder4312(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n0,n2,n2,n3,n1,n3,t2b,d2)
       allocate(u165(n2+1:n3,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k4*k4
       i2=k3*k4
       i3=k2*k1
       call egemm(i1,i2,i3,f1,d2,u165)
       deallocate(f1)
       deallocate(d2)
c
       allocate(x61(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       x61=0.0d0
      call sum341256(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x61,u165,1.000)
       deallocate(u165)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder465123(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n1,u53,f1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(u160(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k4*k1
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,f1,d2,u160)
       deallocate(f1)
       deallocate(d2)
c
       call sum341256(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,
     & x38,u160,-1.000)
       deallocate(u160)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder465123(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n1,u53,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s176(n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k4*k1
       i3=k2*k4
       call egemm1(i1,i3,f1,b2,s176)
       deallocate(f1)
       deallocate(b2)
c
       x1=x1+s176
       deallocate(s176)
c
       allocate(f1(n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder654123(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n2,n0,n1,n2,n3,n2,n3,n0,n2,n0,n1,u53,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u140(n2+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k4*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u140)
       deallocate(f1)
       deallocate(b2)
c
       call sum312456(n0,n1,n2,n3,n2,n3,n2,n3,n0,n2,n0,n1,
     & x55,u140,-1.000)
       deallocate(u140)
c
!       allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder512346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
!       allocate(z132(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k4*k4
!       i2=k1*k2*k3*k3
!       i3=k4*k1
!       call egemm(i1,i2,i3,x55,f2,z132)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n1
             sum=sum
     &     + x55(m,f,d,c,k,i)*t3b(f,b,a,l,m,j)      !mfdckifbalmj    (+1.000)
     &     - x55(m,f,c,d,k,i)*t3b(f,b,a,l,m,j)      !mfcdkifbalmj    (-1.000)
     &     - x55(m,f,d,c,l,i)*t3b(f,b,a,k,m,j)      !mfdclifbakmj    (-1.000)
     &     + x55(m,f,c,d,l,i)*t3b(f,b,a,k,m,j)      !mfcdlifbakmj    (+1.000)
     &     - x55(m,f,d,c,k,j)*t3b(f,b,a,l,m,i)      !mfdckjfbalmi    (-1.000)
     &     + x55(m,f,c,d,k,j)*t3b(f,b,a,l,m,i)      !mfcdkjfbalmi    (+1.000)
     &     + x55(m,f,d,c,l,j)*t3b(f,b,a,k,m,i)      !mfdcljfbakmi    (+1.000)
     &     - x55(m,f,c,d,l,j)*t3b(f,b,a,k,m,i)      !mfcdljfbakmi    (-1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34571268(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z132, 1.000)
!       call sum34572168(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z132,-1.000)
!       call sum34671258(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z132,-1.000)
!       call sum34672158(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z132, 1.000)
!       call sum34581267(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z132,-1.000)
!       call sum34582167(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z132, 1.000)
!       call sum34681257(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z132, 1.000)
!       call sum34682157(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z132,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z132(b,a,l,j,d,c,k,i)       ! 34571268 (+1.000)
!     & -z132(b,a,l,j,c,d,k,i)       ! 34572168 (-1.000)
!     & -z132(b,a,k,j,d,c,l,i)       ! 34671258 (-1.000)
!     & +z132(b,a,k,j,c,d,l,i)       ! 34672158 (+1.000)
!     & -z132(b,a,l,i,d,c,k,j)       ! 34581267 (-1.000)
!     & +z132(b,a,l,i,c,d,k,j)       ! 34582167 (+1.000)
!     & +z132(b,a,k,i,d,c,l,j)       ! 34681257 (+1.000)
!     & -z132(b,a,k,i,c,d,l,j)       ! 34682157 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z132)
       deallocate(x55)
c
       allocate(f1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder451236(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n1,n2,n3,n0,n2,n0,n1,n0,n2,u53,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u138(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k4*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u138)
       deallocate(f1)
       deallocate(b2)
c
      call sum523461(n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,x54,u138,1.000)
c
!       allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder451236(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n0,n1,n2,n3,n1,n3,n1,n3,n0,n1,t3b,f2)
!       allocate(z129(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k4
!       i2=k1*k3*k3*k4
!       i3=k1*k2
!       call egemm(i1,i2,i3,x54,f2,z129)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     - x54(n,m,d,l,k,i)*t3b(c,b,a,n,m,j)      !nmdlkicbanmj    (-1.000)
     &     + x54(n,m,c,l,k,i)*t3b(d,b,a,n,m,j)      !nmclkidbanmj    (+1.000)
     &     + x54(n,m,d,k,l,i)*t3b(c,b,a,n,m,j)      !nmdklicbanmj    (+1.000)
     &     - x54(n,m,c,k,l,i)*t3b(d,b,a,n,m,j)      !nmcklidbanmj    (-1.000)
     &     + x54(n,m,d,l,k,j)*t3b(c,b,a,n,m,i)      !nmdlkjcbanmi    (+1.000)
     &     - x54(n,m,c,l,k,j)*t3b(d,b,a,n,m,i)      !nmclkjdbanmi    (-1.000)
     &     - x54(n,m,d,k,l,j)*t3b(c,b,a,n,m,i)      !nmdkljcbanmi    (-1.000)
     &     + x54(n,m,c,k,l,j)*t3b(d,b,a,n,m,i)      !nmckljdbanmi    (+1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23471568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z129,-1.000)
!       call sum13472568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z129, 1.000)
!       call sum23471658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z129, 1.000)
!       call sum13472658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z129,-1.000)
!       call sum23481567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z129, 1.000)
!       call sum13482567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z129,-1.000)
!       call sum23481657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z129,-1.000)
!       call sum13482657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z129, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z129(c,b,a,j,d,l,k,i)       ! 23471568 (-1.000)
!     & +z129(d,b,a,j,c,l,k,i)       ! 13472568 (+1.000)
!     & +z129(c,b,a,j,d,k,l,i)       ! 23471658 (+1.000)
!     & -z129(d,b,a,j,c,k,l,i)       ! 13472658 (-1.000)
!     & +z129(c,b,a,i,d,l,k,j)       ! 23481567 (+1.000)
!     & -z129(d,b,a,i,c,l,k,j)       ! 13482567 (-1.000)
!     & -z129(c,b,a,i,d,k,l,j)       ! 23481657 (-1.000)
!     & +z129(d,b,a,i,c,k,l,j)       ! 13482657 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z129)
       deallocate(x54)
c
       allocate(f1(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder623415(n0,n2,n0,n1,n2,n3,n0,n2,n0,n1,n0,n2,
     & n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,u138,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u190(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u190)
       deallocate(f1)
       deallocate(b2)
       deallocate(u138)
c
      call sum213456(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x38,u190,1.000)
       deallocate(u190)
c
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z334(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k4*k4
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x38,d2,z334)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - x38(m,d,c,l,k,j)*t2a(b,a,m,i)          !mdclkjbami      (-1.000)
     &     + x38(m,c,d,l,k,j)*t2a(b,a,m,i)          !mcdlkjbami      (+1.000)
     &     + x38(m,d,c,k,l,j)*t2a(b,a,m,i)          !mdckljbami      (+1.000)
     &     - x38(m,c,d,k,l,j)*t2a(b,a,m,i)          !mcdkljbami      (-1.000)
     &     + x38(m,d,c,l,k,i)*t2a(b,a,m,j)          !mdclkibamj      (+1.000)
     &     - x38(m,c,d,l,k,i)*t2a(b,a,m,j)          !mcdlkibamj      (-1.000)
     &     - x38(m,d,c,k,l,i)*t2a(b,a,m,j)          !mdcklibamj      (-1.000)
     &     + x38(m,c,d,k,l,i)*t2a(b,a,m,j)          !mcdklibamj      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34812567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z334,-1.000)
!       call sum34821567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z334, 1.000)
!       call sum34812657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z334, 1.000)
!       call sum34821657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z334,-1.000)
!       call sum34712568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z334, 1.000)
!       call sum34721568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z334,-1.000)
!       call sum34712658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z334,-1.000)
!       call sum34721658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z334, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z334(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & +z334(b,a,i,c,d,l,k,j)       ! 34821567 (+1.000)
!     & +z334(b,a,i,d,c,k,l,j)       ! 34812657 (+1.000)
!     & -z334(b,a,i,c,d,k,l,j)       ! 34821657 (-1.000)
!     & +z334(b,a,j,d,c,l,k,i)       ! 34712568 (+1.000)
!     & -z334(b,a,j,c,d,l,k,i)       ! 34721568 (-1.000)
!     & -z334(b,a,j,d,c,k,l,i)       ! 34712658 (-1.000)
!     & +z334(b,a,j,c,d,k,l,i)       ! 34721658 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z334)
       deallocate(x38)
c
       allocate(f1(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n2,n3,n2,n3,n0,n2,n0,n1,n0,n2,u53,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u111(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k4*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u111)
       deallocate(f1)
       deallocate(b2)
       deallocate(u53)
c
       call sum423561(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,
     & x57,u111,-1.000)
c
       allocate(f1(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder623145(n1,n3,n2,n3,n2,n3,n0,n2,n0,n1,n0,n2,
     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,u111,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u188(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u188)
       deallocate(f1)
       deallocate(b2)
c
       call sum213456(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,
     & x61,u188,-1.000)
       deallocate(u188)
c
!       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
!       allocate(z401(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k4*k4
!       i2=k1*k2*k3
!       i3=k4
!       call egemm(i1,i2,i3,x61,d2,z401)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum
     &     - x61(f,d,c,a,k,i)*t2b(f,b,l,j)          !fdcakifblj      (-1.000)
     &     + x61(f,d,c,b,k,i)*t2b(f,a,l,j)          !fdcbkifalj      (+1.000)
     &     - x61(f,d,c,a,l,j)*t2b(f,b,k,i)          !fdcaljfbki      (-1.000)
     &     + x61(f,d,c,b,l,j)*t2b(f,a,k,i)          !fdcbljfaki      (+1.000)
     &     + x61(f,c,d,a,k,i)*t2b(f,b,l,j)          !fcdakifblj      (+1.000)
     &     - x61(f,c,d,b,k,i)*t2b(f,a,l,j)          !fcdbkifalj      (-1.000)
     &     + x61(f,c,d,a,l,j)*t2b(f,b,k,i)          !fcdaljfbki      (+1.000)
     &     - x61(f,c,d,b,l,j)*t2b(f,a,k,i)          !fcdbljfaki      (-1.000)
     &     + x61(f,d,c,a,l,i)*t2b(f,b,k,j)          !fdcalifbkj      (+1.000)
     &     - x61(f,d,c,b,l,i)*t2b(f,a,k,j)          !fdcblifakj      (-1.000)
     &     + x61(f,d,c,a,k,j)*t2b(f,b,l,i)          !fdcakjfbli      (+1.000)
     &     - x61(f,d,c,b,k,j)*t2b(f,a,l,i)          !fdcbkjfali      (-1.000)
     &     - x61(f,c,d,a,l,i)*t2b(f,b,k,j)          !fcdalifbkj      (-1.000)
     &     + x61(f,c,d,b,l,i)*t2b(f,a,k,j)          !fcdblifakj      (+1.000)
     &     - x61(f,c,d,a,k,j)*t2b(f,b,l,i)          !fcdakjfbli      (-1.000)
     &     + x61(f,c,d,b,k,j)*t2b(f,a,l,i)          !fcdbkjfali      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum35712468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401,-1.000)
!       call sum45712368(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401, 1.000)
!       call sum36812457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401,-1.000)
!       call sum46812357(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401, 1.000)
!       call sum35721468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401, 1.000)
!       call sum45721368(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401,-1.000)
!       call sum36821457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401, 1.000)
!       call sum46821357(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401,-1.000)
!       call sum36712458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401, 1.000)
!       call sum46712358(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401,-1.000)
!       call sum35812467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401, 1.000)
!       call sum45812367(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401,-1.000)
!       call sum36721458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401,-1.000)
!       call sum46721358(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401, 1.000)
!       call sum35821467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401,-1.000)
!       call sum45821367(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z401, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z401(b,l,j,d,c,a,k,i)       ! 35712468 (-1.000)
!     & +z401(a,l,j,d,c,b,k,i)       ! 45712368 (+1.000)
!     & -z401(b,k,i,d,c,a,l,j)       ! 36812457 (-1.000)
!     & +z401(a,k,i,d,c,b,l,j)       ! 46812357 (+1.000)
!     & +z401(b,l,j,c,d,a,k,i)       ! 35721468 (+1.000)
!     & -z401(a,l,j,c,d,b,k,i)       ! 45721368 (-1.000)
!     & +z401(b,k,i,c,d,a,l,j)       ! 36821457 (+1.000)
!     & -z401(a,k,i,c,d,b,l,j)       ! 46821357 (-1.000)
!     & +z401(b,k,j,d,c,a,l,i)       ! 36712458 (+1.000)
!     & -z401(a,k,j,d,c,b,l,i)       ! 46712358 (-1.000)
!     & +z401(b,l,i,d,c,a,k,j)       ! 35812467 (+1.000)
!     & -z401(a,l,i,d,c,b,k,j)       ! 45812367 (-1.000)
!     & -z401(b,k,j,c,d,a,l,i)       ! 36721458 (-1.000)
!     & +z401(a,k,j,c,d,b,l,i)       ! 46721358 (+1.000)
!     & -z401(b,l,i,c,d,a,k,j)       ! 35821467 (-1.000)
!     & +z401(a,l,i,c,d,b,k,j)       ! 45821367 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z401)
       deallocate(x61)
c
       allocate(f1(n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder263145(n1,n3,n2,n3,n2,n3,n0,n2,n0,n1,n0,n2,
     & n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,u111,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u187(n0+1:n2,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u187)
       deallocate(f1)
       deallocate(b2)
       deallocate(u111)
c
      call sum512346(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x28,u187,1.000)
       deallocate(u187)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u54(n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k3
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u54)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder546123(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
!     & n0,n1,n1,n3,n0,n2,n1,n3,n0,n2,n0,n1,u54,f1)
!       allocate(h2(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       call reorder73512468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t4c,h2)
!       allocate(z165(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3
!       i2=k1*k2*k3*k4*k4
!       i3=k2*k3*k1
!       call egemm(i1,i2,i3,f1,h2,z165)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1; do n=n0+1,n2
             sum=sum
     &     - u54(b,k,i,e,m,n)*t4c(d,c,e,a,n,l,m,j)  !bkiemndceanlmj  (-1.000)
     &     + u54(a,k,i,e,m,n)*t4c(d,c,e,b,n,l,m,j)  !akiemndcebnlmj  (+1.000)
     &     + u54(b,l,i,e,m,n)*t4c(d,c,e,a,n,k,m,j)  !bliemndceankmj  (+1.000)
     &     - u54(a,l,i,e,m,n)*t4c(d,c,e,b,n,k,m,j)  !aliemndcebnkmj  (-1.000)
     &     + u54(b,k,j,e,m,n)*t4c(d,c,e,a,n,l,m,i)  !bkjemndceanlmi  (+1.000)
     &     - u54(a,k,j,e,m,n)*t4c(d,c,e,b,n,l,m,i)  !akjemndcebnlmi  (-1.000)
     &     - u54(b,l,j,e,m,n)*t4c(d,c,e,a,n,k,m,i)  !bljemndceankmi  (-1.000)
     &     + u54(a,l,j,e,m,n)*t4c(d,c,e,b,n,k,m,i)  !aljemndcebnkmi  (+1.000)
             enddo;enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12457368(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z165,-1.000)
!       call sum12357468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z165, 1.000)
!       call sum12467358(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z165, 1.000)
!       call sum12367458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z165,-1.000)
!       call sum12458367(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z165, 1.000)
!       call sum12358467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z165,-1.000)
!       call sum12468357(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z165,-1.000)
!       call sum12368457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z165, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z165(d,c,a,l,j,b,k,i)       ! 12457368 (-1.000)
!     & +z165(d,c,b,l,j,a,k,i)       ! 12357468 (+1.000)
!     & +z165(d,c,a,k,j,b,l,i)       ! 12467358 (+1.000)
!     & -z165(d,c,b,k,j,a,l,i)       ! 12367458 (-1.000)
!     & +z165(d,c,a,l,i,b,k,j)       ! 12458367 (+1.000)
!     & -z165(d,c,b,l,i,a,k,j)       ! 12358467 (-1.000)
!     & -z165(d,c,a,k,i,b,l,j)       ! 12468357 (-1.000)
!     & +z165(d,c,b,k,i,a,l,j)       ! 12368457 (+1.000)
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
c
c
       allocate(f1(n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder465123(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
     & n1,n3,n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,u54,f1)
       allocate(d2(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder2314(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n2,n2,n3,n0,n1,t2b,d2)
       allocate(u169(n2+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k1
       i2=k1*k4
       i3=k2*k3
       call egemm(i1,i2,i3,f1,d2,u169)
       deallocate(f1)
       deallocate(d2)
c
      call sum251346(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x27,u169,1.000)
       deallocate(u169)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n1,n3,n0,n2,n0,n1,n0,n2,u54,f1)
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(u168(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,f1,d2,u168)
       deallocate(f1)
       deallocate(d2)
c
       call sum243561(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & x28,u168,-1.000)
       deallocate(u168)
c
       allocate(f1(n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder654123(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
     & n0,n2,n0,n1,n1,n3,n1,n3,n0,n2,n0,n1,u54,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u141(n2+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u141)
       deallocate(f1)
       deallocate(b2)
c
       call sum312456(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,
     & x53,u141,-1.000)
       deallocate(u141)
c
!       allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder521346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
!       allocate(z126(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k4
!       i2=k1*k2*k3*k4
!       i3=k3*k1
!       call egemm(i1,i2,i3,x53,f2,z126)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + x53(m,f,d,b,k,i)*t3b(c,f,a,l,m,j)      !mfdbkicfalmj    (+1.000)
     &     - x53(m,f,d,a,k,i)*t3b(c,f,b,l,m,j)      !mfdakicfblmj    (-1.000)
     &     - x53(m,f,c,b,k,i)*t3b(d,f,a,l,m,j)      !mfcbkidfalmj    (-1.000)
     &     + x53(m,f,c,a,k,i)*t3b(d,f,b,l,m,j)      !mfcakidfblmj    (+1.000)
     &     - x53(m,f,d,b,l,i)*t3b(c,f,a,k,m,j)      !mfdblicfakmj    (-1.000)
     &     + x53(m,f,d,a,l,i)*t3b(c,f,b,k,m,j)      !mfdalicfbkmj    (+1.000)
     &     + x53(m,f,c,b,l,i)*t3b(d,f,a,k,m,j)      !mfcblidfakmj    (+1.000)
     &     - x53(m,f,c,a,l,i)*t3b(d,f,b,k,m,j)      !mfcalidfbkmj    (-1.000)
     &     - x53(m,f,d,b,k,j)*t3b(c,f,a,l,m,i)      !mfdbkjcfalmi    (-1.000)
     &     + x53(m,f,d,a,k,j)*t3b(c,f,b,l,m,i)      !mfdakjcfblmi    (+1.000)
     &     + x53(m,f,c,b,k,j)*t3b(d,f,a,l,m,i)      !mfcbkjdfalmi    (+1.000)
     &     - x53(m,f,c,a,k,j)*t3b(d,f,b,l,m,i)      !mfcakjdfblmi    (-1.000)
     &     + x53(m,f,d,b,l,j)*t3b(c,f,a,k,m,i)      !mfdbljcfakmi    (+1.000)
     &     - x53(m,f,d,a,l,j)*t3b(c,f,b,k,m,i)      !mfdaljcfbkmi    (-1.000)
     &     - x53(m,f,c,b,l,j)*t3b(d,f,a,k,m,i)      !mfcbljdfakmi    (-1.000)
     &     + x53(m,f,c,a,l,j)*t3b(d,f,b,k,m,i)      !mfcaljdfbkmi    (+1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum24571368(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126, 1.000)
!       call sum23571468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126,-1.000)
!       call sum14572368(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126,-1.000)
!       call sum13572468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126, 1.000)
!       call sum24671358(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126,-1.000)
!       call sum23671458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126, 1.000)
!       call sum14672358(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126, 1.000)
!       call sum13672458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126,-1.000)
!       call sum24581367(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126,-1.000)
!       call sum23581467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126, 1.000)
!       call sum14582367(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126, 1.000)
!       call sum13582467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126,-1.000)
!       call sum24681357(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126, 1.000)
!       call sum23681457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126,-1.000)
!       call sum14682357(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126,-1.000)
!       call sum13682457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z126, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z126(c,a,l,j,d,b,k,i)       ! 24571368 (+1.000)
!     & -z126(c,b,l,j,d,a,k,i)       ! 23571468 (-1.000)
!     & -z126(d,a,l,j,c,b,k,i)       ! 14572368 (-1.000)
!     & +z126(d,b,l,j,c,a,k,i)       ! 13572468 (+1.000)
!     & -z126(c,a,k,j,d,b,l,i)       ! 24671358 (-1.000)
!     & +z126(c,b,k,j,d,a,l,i)       ! 23671458 (+1.000)
!     & +z126(d,a,k,j,c,b,l,i)       ! 14672358 (+1.000)
!     & -z126(d,b,k,j,c,a,l,i)       ! 13672458 (-1.000)
!     & -z126(c,a,l,i,d,b,k,j)       ! 24581367 (-1.000)
!     & +z126(c,b,l,i,d,a,k,j)       ! 23581467 (+1.000)
!     & +z126(d,a,l,i,c,b,k,j)       ! 14582367 (+1.000)
!     & -z126(d,b,l,i,c,a,k,j)       ! 13582467 (-1.000)
!     & +z126(c,a,k,i,d,b,l,j)       ! 24681357 (+1.000)
!     & -z126(c,b,k,i,d,a,l,j)       ! 23681457 (-1.000)
!     & -z126(d,a,k,i,c,b,l,j)       ! 14682357 (-1.000)
!     & +z126(d,b,k,i,c,a,l,j)       ! 13682457 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z126)
       deallocate(x53)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n1,n3,n0,n2,n0,n1,n0,n2,u54,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s150(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3
       i3=k3*k1
       call egemm1(i1,i3,f1,b2,s150)
       deallocate(f1)
       deallocate(b2)
c
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x7,s150, 1.000)
       deallocate(s150)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n1,n3,n0,n2,n0,n1,n0,n2,u54,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u112(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u112)
       deallocate(f1)
       deallocate(b2)
c
       call sum324561(n0,n2,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,
     & x58,u112,-1.000)
       deallocate(u112)
c
!       allocate(f2(n0+1:n2,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder431256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n1,n3,n2,n3,n2,n3,n0,n2,n0,n1,t3c,f2)
!       allocate(z150(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k3
!       i2=k1*k2*k4*k4
!       i3=k3*k2
!       call egemm(i1,i2,i3,x58,f2,z150)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n2
             sum=sum
     &     + x58(m,e,b,a,k,i)*t3c(d,c,e,m,l,j)      !mebakidcemlj    (+1.000)
     &     - x58(m,e,a,b,k,i)*t3c(d,c,e,m,l,j)      !meabkidcemlj    (-1.000)
     &     - x58(m,e,b,a,l,i)*t3c(d,c,e,m,k,j)      !mebalidcemkj    (-1.000)
     &     + x58(m,e,a,b,l,i)*t3c(d,c,e,m,k,j)      !meablidcemkj    (+1.000)
     &     - x58(m,e,b,a,k,j)*t3c(d,c,e,m,l,i)      !mebakjdcemli    (-1.000)
     &     + x58(m,e,a,b,k,j)*t3c(d,c,e,m,l,i)      !meabkjdcemli    (+1.000)
     &     + x58(m,e,b,a,l,j)*t3c(d,c,e,m,k,i)      !mebaljdcemki    (+1.000)
     &     - x58(m,e,a,b,l,j)*t3c(d,c,e,m,k,i)      !meabljdcemki    (-1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12573468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z150, 1.000)
!       call sum12574368(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z150,-1.000)
!       call sum12673458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z150,-1.000)
!       call sum12674358(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z150, 1.000)
!       call sum12583467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z150,-1.000)
!       call sum12584367(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z150, 1.000)
!       call sum12683457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z150, 1.000)
!       call sum12684357(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z150,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z150(d,c,l,j,b,a,k,i)       ! 12573468 (+1.000)
!     & -z150(d,c,l,j,a,b,k,i)       ! 12574368 (-1.000)
!     & -z150(d,c,k,j,b,a,l,i)       ! 12673458 (-1.000)
!     & +z150(d,c,k,j,a,b,l,i)       ! 12674358 (+1.000)
!     & -z150(d,c,l,i,b,a,k,j)       ! 12583467 (-1.000)
!     & +z150(d,c,l,i,a,b,k,j)       ! 12584367 (+1.000)
!     & +z150(d,c,k,i,b,a,l,j)       ! 12683457 (+1.000)
!     & -z150(d,c,k,i,a,b,l,j)       ! 12684357 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z150)
       deallocate(x58)
c
       allocate(f1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder451236(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
     & n1,n3,n0,n1,n1,n3,n0,n2,n0,n1,n0,n2,u54,f1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(u108(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,f1,b2,u108)
       deallocate(f1)
       deallocate(b2)
       deallocate(u54)
c
      call sum623451(n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,x56,u108,1.000)
c
!       allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder461235(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,t3c,f2)
!       allocate(z143(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3
!       i2=k2*k3*k4*k4
!       i3=k1*k2
!       call egemm(i1,i2,i3,x56,f2,z143)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     - x56(n,m,b,k,j,i)*t3c(d,c,a,n,l,m)      !nmbkjidcanlm    (-1.000)
     &     + x56(n,m,a,k,j,i)*t3c(d,c,b,n,l,m)      !nmakjidcbnlm    (+1.000)
     &     + x56(n,m,b,l,j,i)*t3c(d,c,a,n,k,m)      !nmbljidcankm    (+1.000)
     &     - x56(n,m,a,l,j,i)*t3c(d,c,b,n,k,m)      !nmaljidcbnkm    (-1.000)
     &     + x56(n,m,b,k,i,j)*t3c(d,c,a,n,l,m)      !nmbkijdcanlm    (+1.000)
     &     - x56(n,m,a,k,i,j)*t3c(d,c,b,n,l,m)      !nmakijdcbnlm    (-1.000)
     &     - x56(n,m,b,l,i,j)*t3c(d,c,a,n,k,m)      !nmblijdcankm    (-1.000)
     &     + x56(n,m,a,l,i,j)*t3c(d,c,b,n,k,m)      !nmalijdcbnkm    (+1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12453678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z143,-1.000)
!       call sum12354678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z143, 1.000)
!       call sum12463578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z143, 1.000)
!       call sum12364578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z143,-1.000)
!       call sum12453687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z143, 1.000)
!       call sum12354687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z143,-1.000)
!       call sum12463587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z143,-1.000)
!       call sum12364587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z143, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z143(d,c,a,l,b,k,j,i)       ! 12453678 (-1.000)
!     & +z143(d,c,b,l,a,k,j,i)       ! 12354678 (+1.000)
!     & +z143(d,c,a,k,b,l,j,i)       ! 12463578 (+1.000)
!     & -z143(d,c,b,k,a,l,j,i)       ! 12364578 (-1.000)
!     & +z143(d,c,a,l,b,k,i,j)       ! 12453687 (+1.000)
!     & -z143(d,c,b,l,a,k,i,j)       ! 12354687 (-1.000)
!     & -z143(d,c,a,k,b,l,i,j)       ! 12463587 (-1.000)
!     & +z143(d,c,b,k,a,l,i,j)       ! 12364587 (+1.000)
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
       deallocate(x56)
c
       allocate(f1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder623451(n0,n1,n0,n1,n1,n3,n0,n2,n0,n1,n0,n2,
     & n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,u108,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u186(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u186)
       deallocate(f1)
       deallocate(b2)
c
       call sum213456(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x27,u186,-1.000)
       deallocate(u186)
c
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z279(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k4
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x27,d2,z279)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     + x27(n,d,b,k,j,i)*t2b(c,a,l,n)          !ndbkjicaln      (+1.000)
     &     - x27(n,d,a,k,j,i)*t2b(c,b,l,n)          !ndakjicbln      (-1.000)
     &     - x27(n,c,b,k,j,i)*t2b(d,a,l,n)          !ncbkjidaln      (-1.000)
     &     + x27(n,c,a,k,j,i)*t2b(d,b,l,n)          !ncakjidbln      (+1.000)
     &     - x27(n,d,b,l,j,i)*t2b(c,a,k,n)          !ndbljicakn      (-1.000)
     &     + x27(n,d,a,l,j,i)*t2b(c,b,k,n)          !ndaljicbkn      (+1.000)
     &     + x27(n,c,b,l,j,i)*t2b(d,a,k,n)          !ncbljidakn      (+1.000)
     &     - x27(n,c,a,l,j,i)*t2b(d,b,k,n)          !ncaljidbkn      (-1.000)
     &     - x27(n,d,b,k,i,j)*t2b(c,a,l,n)          !ndbkijcaln      (-1.000)
     &     + x27(n,d,a,k,i,j)*t2b(c,b,l,n)          !ndakijcbln      (+1.000)
     &     + x27(n,c,b,k,i,j)*t2b(d,a,l,n)          !ncbkijdaln      (+1.000)
     &     - x27(n,c,a,k,i,j)*t2b(d,b,l,n)          !ncakijdbln      (-1.000)
     &     + x27(n,d,b,l,i,j)*t2b(c,a,k,n)          !ndblijcakn      (+1.000)
     &     - x27(n,d,a,l,i,j)*t2b(c,b,k,n)          !ndalijcbkn      (-1.000)
     &     - x27(n,c,b,l,i,j)*t2b(d,a,k,n)          !ncblijdakn      (-1.000)
     &     + x27(n,c,a,l,i,j)*t2b(d,b,k,n)          !ncalijdbkn      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum24513678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279, 1.000)
!       call sum23514678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279,-1.000)
!       call sum14523678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279,-1.000)
!       call sum13524678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279, 1.000)
!       call sum24613578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279,-1.000)
!       call sum23614578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279, 1.000)
!       call sum14623578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279, 1.000)
!       call sum13624578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279,-1.000)
!       call sum24513687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279,-1.000)
!       call sum23514687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279, 1.000)
!       call sum14523687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279, 1.000)
!       call sum13524687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279,-1.000)
!       call sum24613587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279, 1.000)
!       call sum23614587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279,-1.000)
!       call sum14623587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279,-1.000)
!       call sum13624587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z279, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z279(c,a,l,d,b,k,j,i)       ! 24513678 (+1.000)
!     & -z279(c,b,l,d,a,k,j,i)       ! 23514678 (-1.000)
!     & -z279(d,a,l,c,b,k,j,i)       ! 14523678 (-1.000)
!     & +z279(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & -z279(c,a,k,d,b,l,j,i)       ! 24613578 (-1.000)
!     & +z279(c,b,k,d,a,l,j,i)       ! 23614578 (+1.000)
!     & +z279(d,a,k,c,b,l,j,i)       ! 14623578 (+1.000)
!     & -z279(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!     & -z279(c,a,l,d,b,k,i,j)       ! 24513687 (-1.000)
!     & +z279(c,b,l,d,a,k,i,j)       ! 23514687 (+1.000)
!     & +z279(d,a,l,c,b,k,i,j)       ! 14523687 (+1.000)
!     & -z279(d,b,l,c,a,k,i,j)       ! 13524687 (-1.000)
!     & +z279(c,a,k,d,b,l,i,j)       ! 24613587 (+1.000)
!     & -z279(c,b,k,d,a,l,i,j)       ! 23614587 (-1.000)
!     & -z279(d,a,k,c,b,l,i,j)       ! 14623587 (-1.000)
!     & +z279(d,b,k,c,a,l,i,j)       ! 13624587 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z279)
       deallocate(x27)
c
       allocate(f1(n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder263451(n0,n1,n0,n1,n1,n3,n0,n2,n0,n1,n0,n2,
     & n0,n1,n0,n2,n1,n3,n0,n2,n0,n1,n0,n1,u108,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u179(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k3*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u179)
       deallocate(f1)
       deallocate(b2)
       deallocate(u108)
c
      call sum213456(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x35,u179,1.000)
       deallocate(u179)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s74(n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k2
       i3=k3*k4
       call egemm(i1,i2,i3,d1,d2,s74)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n0,n1,n0,n2,n0,n1,x17,s74, 1.000)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder3412(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s74,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u170(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u170)
       deallocate(d1)
       deallocate(d2)
c
      call sum234156(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x28,u170,1.000)
       deallocate(u170)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder3412(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s74,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u161(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u161)
       deallocate(d1)
       deallocate(d2)
c
       call sum236145(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x29,u161,-1.000)
       deallocate(u161)
c
       allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       call reorder4312(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n2,n0,n1,n0,n2,n0,n1,s74,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s175(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s175)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s175,-1.000)
       deallocate(s175)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder3412(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s74,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s149(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s149)
       deallocate(d1)
       deallocate(b2)
       deallocate(s74)
c
       call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x7,s149,-1.000)
       deallocate(s149)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(h2(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
     & n0+1:n2,n0+1:n1))
       call reorder13724568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n1,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t4c,h2)
       allocate(u55(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k2*k2*k3*k4
       i3=k1*k3*k4
       call egemm(i1,i2,i3,d1,h2,u55)
       deallocate(d1)
       deallocate(h2)
c
      call sum234561(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x34,u55,-1.000)
       deallocate(u55)
c
       allocate(d1(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n2,n3,n0,n1,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder2314(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n2,n2,n3,n0,n1,t2b,d2)
       allocate(s75(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4
       i2=k1*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,d2,s75)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n2,n3,n2,n3,n0,n1,x18,s75,-1.000)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder3412(n2,n3,n0,n1,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s75,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u163(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u163)
       deallocate(d1)
       deallocate(d2)
c
       call sum345126(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,
     & x30,u163,-1.000)
       deallocate(u163)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder3412(n2,n3,n0,n1,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s75,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s174(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s174)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n2,n3,n0,n2,n0,n1,x1,s174,-1.000)
       deallocate(s174)
c
       allocate(d1(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       call reorder4312(n2,n3,n0,n1,n2,n3,n0,n1,
     & n0,n1,n2,n3,n2,n3,n0,n1,s75,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s151(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s151)
       deallocate(d1)
       deallocate(b2)
       deallocate(s75)
c
       call sum3124(n2,n3,n2,n3,n1,n3,n0,n1,x8,s151, 1.000)
       deallocate(s151)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s76(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s76)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n1,n3,n0,n1,x15,s76, 1.000)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n1,n3,n0,n1,n1,n3,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s76,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s153(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s153)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n1,n3,n1,n3,n1,n3,n0,n1,x26,s153,-1.000)
       deallocate(s153)
c
!       allocate(f2(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder312456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
!       allocate(z29(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n1+1:n3,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k2*k2*k4*k4
!       i3=k3
!       call egemm(i1,i2,i3,x26,f2,z29)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + x26(e,b,a,i)*t3c(d,c,e,l,k,j)          !ebaidcelkj      (+1.000)
     &     - x26(e,a,b,i)*t3c(d,c,e,l,k,j)          !eabidcelkj      (-1.000)
     &     - x26(e,b,a,j)*t3c(d,c,e,l,k,i)          !ebajdcelki      (-1.000)
     &     + x26(e,a,b,j)*t3c(d,c,e,l,k,i)          !eabjdcelki      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12567348(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z29, 1.000)
!       call sum12567438(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z29,-1.000)
!       call sum12568347(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z29,-1.000)
!       call sum12568437(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z29, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z29(d,c,l,k,j,b,a,i)       ! 12567348 (+1.000)
!     & -z29(d,c,l,k,j,a,b,i)       ! 12567438 (-1.000)
!     & -z29(d,c,l,k,i,b,a,j)       ! 12568347 (-1.000)
!     & +z29(d,c,l,k,i,a,b,j)       ! 12568437 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z29)
       deallocate(x26)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s76,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s146(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s146)
       deallocate(d1)
       deallocate(b2)
       deallocate(s76)
c
       call sum4123(n0,n1,n1,n3,n0,n1,n0,n1,x25,s146,-1.000)
       deallocate(s146)
c
!       allocate(f2(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
!       call reorder612345(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,t3c,f2)
!       allocate(z28(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k2*k2*k3*k4*k4
!       i3=k1
!       call egemm(i1,i2,i3,x25,f2,z28)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x25(m,b,j,i)*t3c(d,c,a,l,k,m)          !mbjidcalkm      (+1.000)
     &     - x25(m,a,j,i)*t3c(d,c,b,l,k,m)          !majidcblkm      (-1.000)
     &     - x25(m,b,i,j)*t3c(d,c,a,l,k,m)          !mbijdcalkm      (-1.000)
     &     + x25(m,a,i,j)*t3c(d,c,b,l,k,m)          !maijdcblkm      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12456378(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z28, 1.000)
!       call sum12356478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z28,-1.000)
!       call sum12456387(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z28,-1.000)
!       call sum12356487(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z28, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z28(d,c,a,l,k,b,j,i)       ! 12456378 (+1.000)
!     & -z28(d,c,b,l,k,a,j,i)       ! 12356478 (-1.000)
!     & -z28(d,c,a,l,k,b,i,j)       ! 12456387 (-1.000)
!     & +z28(d,c,b,l,k,a,i,j)       ! 12356487 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z28)
       deallocate(x25)
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
       call sum21(n0,n1,n0,n1,x10,q15, 1.000)
       deallocate(q15)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1)
       allocate(h2(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
     & n0+1:n1,n0+1:n1))
       call reorder13524678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4c,h2)
       allocate(u56(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k2*k3*k4
       i3=k2*k3*k4
       call egemm(i1,i2,i3,d1,h2,u56)
       deallocate(d1)
       deallocate(h2)
c
      call sum234561(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x39,u56,-1.000)
       deallocate(u56)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,t2b,d2)
       allocate(s77(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k2*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s77)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n2,n3,n0,n2,x22,s77, 1.000)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder4312(n2,n3,n0,n2,n2,n3,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,s77,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s179(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s179)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n2,n3,n0,n2,x37,s179,-1.000)
       deallocate(s179)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3412(n2,n3,n0,n2,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,s77,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s177(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s177)
       deallocate(d1)
       deallocate(b2)
       deallocate(s77)
c
       call sum4123(n0,n2,n2,n3,n0,n2,n0,n2,x36,s177,-1.000)
       deallocate(s177)
c
       allocate(d1(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n1,n3,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder1423(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n1,n1,n3,n0,n2,t2b,d2)
       allocate(s78(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k2*k3
       i3=k1*k4
       call egemm(i1,i2,i3,d1,d2,s78)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n1,n3,n1,n3,n0,n2,x19,s78,-1.000)
c
       allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n1,n3,n0,n2,n1,n3,n0,n2,
     & n0,n2,n1,n3,n1,n3,n0,n2,s78,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s180(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s180)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s180, 1.000)
       deallocate(s180)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder3412(n1,n3,n0,n2,n1,n3,n0,n2,
     & n1,n3,n0,n2,n1,n3,n0,n2,s78,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s147(n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s147)
       deallocate(d1)
       deallocate(b2)
       deallocate(s78)
c
       call sum4123(n0,n2,n1,n3,n0,n2,n0,n1,x7,s147,-1.000)
       deallocate(s147)
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
       call sum21(n0,n2,n0,n2,x12,q16, 1.000)
       deallocate(q16)
c
       allocate(d1(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n2,n3,n1,n3,intm,d1)
       allocate(d2(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder4312(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n0,n2,n2,n3,n1,n3,t2b,d2)
       allocate(s79(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4
       i2=k3*k4
       i3=k2*k1
       call egemm(i1,i2,i3,d1,d2,s79)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n2,n3,n1,n3,n2,n3,n1,n3,x20,s79, 1.000)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       call reorder3412(n2,n3,n1,n3,n2,n3,n1,n3,
     & n2,n3,n1,n3,n2,n3,n1,n3,s79,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s178(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s178)
       deallocate(d1)
       deallocate(b2)
c
       call sum4123(n1,n3,n2,n3,n1,n3,n0,n2,x2,s178, 1.000)
       deallocate(s178)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       call reorder4312(n2,n3,n1,n3,n2,n3,n1,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,s79,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s148(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s148)
       deallocate(d1)
       deallocate(b2)
       deallocate(s79)
c
       call sum4123(n2,n3,n2,n3,n1,n3,n0,n1,x8,s148, 1.000)
       deallocate(s148)
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
       call sum21(n2,n3,n2,n3,x13,q17,-1.000)
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
       call sum21(n1,n3,n1,n3,x11,q18,-1.000)
       deallocate(q18)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u57(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u57)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder546123(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n0,n2,n1,n3,n0,n2,n0,n1,u57,f1)
!       allocate(h2(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       call reorder61523478(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n2,n2,n3,n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t4d,h2)
!       allocate(z178(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3
!       i2=k1*k2*k3*k4*k4
!       i3=k2*k4*k2
!       call egemm(i1,i2,i3,f1,h2,z178)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n2;do n=n0+1,n2
             sum=sum
     &     + (u57(a,k,i,f,m,n)*t4d(f,d,c,b,n,m,l,j)       !akifmnfdcbnmlj  (+0.500)
     &     - u57(b,k,i,f,m,n)*t4d(f,d,c,a,n,m,l,j)        !bkifmnfdcanmlj  (-0.500)
     &     + u57(b,l,i,f,m,n)*t4d(f,d,c,a,n,m,k,j)        !blifmnfdcanmkj  (+0.500)
     &     - u57(a,l,i,f,m,n)*t4d(f,d,c,b,n,m,k,j)        !alifmnfdcbnmkj  (-0.500)
     &     + u57(b,k,j,f,m,n)*t4d(f,d,c,a,n,m,l,i)        !bkjfmnfdcanmli  (+0.500)
     &     - u57(a,k,j,f,m,n)*t4d(f,d,c,b,n,m,l,i)        !akjfmnfdcbnmli  (-0.500)
     &     - u57(b,l,j,f,m,n)*t4d(f,d,c,a,n,m,k,i)        !bljfmnfdcanmki  (-0.500)
     &     + u57(a,l,j,f,m,n)*t4d(f,d,c,b,n,m,k,i))/2.0d0 !aljfmnfdcbnmki  (+0.500)
             enddo;enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12457368(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z178,-0.500)
!       call sum12357468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z178, 0.500)
!       call sum12467358(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z178, 0.500)
!       call sum12367458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z178,-0.500)
!       call sum12458367(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z178, 0.500)
!       call sum12358467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z178,-0.500)
!       call sum12468357(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z178,-0.500)
!       call sum12368457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z178, 0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z178(d,c,b,l,j,a,k,i)      ! 12357468 (+0.500) top two switched
!     & -z178(d,c,a,l,j,b,k,i)       ! 12457368 (-0.500)
!     & +z178(d,c,a,k,j,b,l,i)       ! 12467358 (+0.500)
!     & -z178(d,c,b,k,j,a,l,i)       ! 12367458 (-0.500)
!     & +z178(d,c,a,l,i,b,k,j)       ! 12458367 (+0.500)
!     & -z178(d,c,b,l,i,a,k,j)       ! 12358467 (-0.500)
!     & -z178(d,c,a,k,i,b,l,j)       ! 12468357 (-0.500)
!     & +z178(d,c,b,k,i,a,l,j))/2.0d0! 12368457 (+0.500)
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
c
       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,n0,n2,u57,f1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(u173(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,f1,d2,u173)
       deallocate(f1)
       deallocate(d2)
c
      call sum352461(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x35,u173,1.000)
       deallocate(u173)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z286(n2+1:n3,n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k3
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x35,d2,z286)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     + x35(n,b,a,k,j,i)*t2c(d,c,n,l)          !nbakjidcnl      (+1.000)
     &     - x35(n,a,b,k,j,i)*t2c(d,c,n,l)          !nabkjidcnl      (-1.000)
     &     - x35(n,b,a,l,j,i)*t2c(d,c,n,k)          !nbaljidcnk      (-1.000)
     &     + x35(n,a,b,l,j,i)*t2c(d,c,n,k)          !nabljidcnk      (+1.000)
     &     - x35(n,b,a,k,i,j)*t2c(d,c,n,l)          !nbakijdcnl      (-1.000)
     &     + x35(n,a,b,k,i,j)*t2c(d,c,n,l)          !nabkijdcnl      (+1.000)
     &     + x35(n,b,a,l,i,j)*t2c(d,c,n,k)          !nbalijdcnk      (+1.000)
     &     - x35(n,a,b,l,i,j)*t2c(d,c,n,k)          !nablijdcnk      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12534678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z286, 1.000)
!       call sum12543678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z286,-1.000)
!       call sum12634578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z286,-1.000)
!       call sum12643578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z286, 1.000)
!       call sum12534687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z286,-1.000)
!       call sum12543687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z286, 1.000)
!       call sum12634587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z286, 1.000)
!       call sum12643587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z286,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z286(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & -z286(d,c,l,a,b,k,j,i)       ! 12543678 (-1.000)
!     & -z286(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & +z286(d,c,k,a,b,l,j,i)       ! 12643578 (+1.000)
!     & -z286(d,c,l,b,a,k,i,j)       ! 12534687 (-1.000)
!     & +z286(d,c,l,a,b,k,i,j)       ! 12543687 (+1.000)
!     & +z286(d,c,k,b,a,l,i,j)       ! 12634587 (+1.000)
!     & -z286(d,c,k,a,b,l,i,j)       ! 12643587 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z286)
       deallocate(x35)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder465123(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n0,n2,n1,n3,n0,n2,n0,n1,u57,f1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(u172(n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k2
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,f1,d2,u172)
       deallocate(f1)
       deallocate(d2)
c
      call sum241356(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x40,u172,1.000)
       deallocate(u172)
c
       allocate(f1(n0+1:n2,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder564123(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,u57,f1)
       allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder4312(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
       allocate(u171(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k4
       i2=k4*k4
       i3=k2*k2
       call egemm(i1,i2,i3,f1,d2,u171)
       deallocate(f1)
       deallocate(d2)
c
      call sum231456(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x50,u171,0.500)
       deallocate(u171)
c
       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,n0,n2,u57,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s183(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3
       i3=k4*k2
       call egemm1(i1,i3,f1,b2,s183)
       deallocate(f1)
       deallocate(b2)
c
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x7,s183,-1.000)
       deallocate(s183)
c
       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,n0,n2,u57,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u146(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u146)
       deallocate(f1)
       deallocate(b2)
c
       call sum324561(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,
     & x57,u146,-1.000)
c
       allocate(f1(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder621345(n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,n0,n2,
     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,u146,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u193(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u193)
       deallocate(f1)
       deallocate(b2)
       deallocate(u146)
c
      call sum213456(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x50,u193,1.000)
       deallocate(u193)
c
!       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
!       allocate(z118(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k4*k4
!       i2=k1*k2*k3
!       i3=k4
!       call egemm(i1,i2,i3,x50,d2,z118)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum
     &     + x50(f,d,c,a,l,j)*t2b(f,b,k,i)          !fdcaljfbki      (+1.000)
     &     + x50(f,d,c,a,k,i)*t2b(f,b,l,j)          !fdcakifblj      (+1.000)
     &     - x50(f,d,c,a,k,j)*t2b(f,b,l,i)          !fdcakjfbli      (-1.000)
     &     - x50(f,d,c,a,l,i)*t2b(f,b,k,j)          !fdcalifbkj      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum36812457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z118, 1.000)
!       call sum35712468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z118, 1.000)
!       call sum35812467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z118,-1.000)
!       call sum36712458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z118,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z118(b,k,i,d,c,a,l,j)       ! 36812457 (+1.000)
!     & +z118(b,l,j,d,c,a,k,i)       ! 35712468 (+1.000)
!     & -z118(b,l,i,d,c,a,k,j)       ! 35812467 (-1.000)
!     & -z118(b,k,j,d,c,a,l,i)       ! 36712458 (-1.000)
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
       deallocate(x50)
c
       allocate(f1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder451236(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n2,n0,n1,n0,n2,u57,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u144(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u144)
       deallocate(f1)
       deallocate(b2)
       deallocate(u57)
c
       call sum523461(n0,n2,n0,n2,n1,n3,n0,n2,n0,n2,n0,n1,
     & x60,u144,-1.000)
c
!       allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder451236(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n0,n2,n2,n3,n2,n3,n1,n3,n0,n1,t3c,f2)
!       allocate(z155(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3
!       i2=k1*k3*k4*k4
!       i3=k2*k2
!       call egemm(i1,i2,i3,x60,f2,z155)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2;do n=n0+1,n2
             sum=sum                                 !top two switched
     &     + (x60(n,m,a,l,k,i)*t3c(d,c,b,n,m,j)       !nmalkidcbnmj    (+0.500)
     &     - x60(n,m,b,l,k,i)*t3c(d,c,a,n,m,j)        !nmblkidcanmj    (-0.500)
     &     + x60(n,m,b,k,l,i)*t3c(d,c,a,n,m,j)        !nmbklidcanmj    (+0.500)
     &     - x60(n,m,a,k,l,i)*t3c(d,c,b,n,m,j)        !nmaklidcbnmj    (-0.500)
     &     + x60(n,m,b,l,k,j)*t3c(d,c,a,n,m,i)        !nmblkjdcanmi    (+0.500)
     &     - x60(n,m,a,l,k,j)*t3c(d,c,b,n,m,i)        !nmalkjdcbnmi    (-0.500)
     &     - x60(n,m,b,k,l,j)*t3c(d,c,a,n,m,i)        !nmbkljdcanmi    (-0.500)
     &     + x60(n,m,a,k,l,j)*t3c(d,c,b,n,m,i))/2.0d0 !nmakljdcbnmi    (+0.500)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12473568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z155,-0.500)
!       call sum12374568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z155, 0.500)
!       call sum12473658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z155, 0.500)
!       call sum12374658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z155,-0.500)
!       call sum12483567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z155, 0.500)
!       call sum12384567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z155,-0.500)
!       call sum12483657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z155,-0.500)
!       call sum12384657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z155, 0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z155(d,c,b,j,a,l,k,i)      ! 12374568 (+0.500) top two switched
!     & -z155(d,c,a,j,b,l,k,i)       ! 12473568 (-0.500)
!     & +z155(d,c,a,j,b,k,l,i)       ! 12473658 (+0.500)
!     & -z155(d,c,b,j,a,k,l,i)       ! 12374658 (-0.500)
!     & +z155(d,c,a,i,b,l,k,j)       ! 12483567 (+0.500)
!     & -z155(d,c,b,i,a,l,k,j)       ! 12384567 (-0.500)
!     & -z155(d,c,a,i,b,k,l,j)       ! 12483657 (-0.500)
!     & +z155(d,c,b,i,a,k,l,j))/2.0d0! 12384657 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z155)
       deallocate(x60)
c
c
       allocate(f1(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder263415(n0,n2,n0,n2,n1,n3,n0,n2,n0,n1,n0,n2,
     & n0,n2,n0,n2,n1,n3,n0,n2,n0,n2,n0,n1,u144,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u192(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2*k3*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u192)
       deallocate(f1)
       deallocate(b2)
       deallocate(u144)
c
      call sum213456(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x28,u192,1.000)
       deallocate(u192)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(h2(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
     & n0+1:n2,n0+1:n1))
       call reorder12534678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
     & n0,n1,n2,n3,n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t4d,h2)
       allocate(u58(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k2*k2*k3*k4
       i3=k2*k4*k4
       call egemm(i1,i2,i3,d1,h2,u58)
       deallocate(d1)
       deallocate(h2)
c
       call sum234561(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x59,u58,0.500)
       deallocate(u58)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z152(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x59,d2,z152)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     - x59(m,c,b,l,k,j)*t2b(d,a,m,i)          !mcblkjdami      (-1.000)
     &     + x59(m,c,a,l,k,j)*t2b(d,b,m,i)          !mcalkjdbmi      (+1.000)
     &     + x59(m,d,b,l,k,j)*t2b(c,a,m,i)          !mdblkjcami      (+1.000)
     &     - x59(m,d,a,l,k,j)*t2b(c,b,m,i)          !mdalkjcbmi      (-1.000)
     &     + x59(m,c,b,l,k,i)*t2b(d,a,m,j)          !mcblkidamj      (+1.000)
     &     - x59(m,c,a,l,k,i)*t2b(d,b,m,j)          !mcalkidbmj      (-1.000)
     &     - x59(m,d,b,l,k,i)*t2b(c,a,m,j)          !mdblkicamj      (-1.000)
     &     + x59(m,d,a,l,k,i)*t2b(c,b,m,j)          !mdalkicbmj      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14823567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z152,-1.000)
!       call sum13824567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z152, 1.000)
!       call sum24813567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z152, 1.000)
!       call sum23814567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z152,-1.000)
!       call sum14723568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z152, 1.000)
!       call sum13724568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z152,-1.000)
!       call sum24713568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z152,-1.000)
!       call sum23714568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z152, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z152(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & +z152(d,b,i,c,a,l,k,j)       ! 13824567 (+1.000)
!     & +z152(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z152(c,b,i,d,a,l,k,j)       ! 23814567 (-1.000)
!     & +z152(d,a,j,c,b,l,k,i)       ! 14723568 (+1.000)
!     & -z152(d,b,j,c,a,l,k,i)       ! 13724568 (-1.000)
!     & -z152(c,a,j,d,b,l,k,i)       ! 24713568 (-1.000)
!     & +z152(c,b,j,d,a,l,k,i)       ! 23714568 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z152)
       deallocate(x59)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s80(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s80)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n1,n3,n0,n1,x24,s80, 1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s80,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u175(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u175)
       deallocate(d1)
       deallocate(d2)
c
       call sum245136(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & x34,u175,-1.000)
       deallocate(u175)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n1,n3,n0,n1,n2,n3,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n1,s80,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s184(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s184)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x8,s184,-1.000)
       deallocate(s184)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s80,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s182(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s182)
       deallocate(d1)
       deallocate(b2)
       deallocate(s80)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x7,s182, 1.000)
       deallocate(s182)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(u59(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k1*k3*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,f2,u59)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x62(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x62=0.0d0
       call sum235641(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x62,u59,1.000)
       deallocate(u59)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u60(n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k3
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u60)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x63(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       x63=0.0d0
       call sum456231(n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,x63,u60,1.000)
       deallocate(u60)
c
       allocate(b1(n2+1:n3,n0+1:n2))
       call reorder12(0,n3,0,n3,
     & n2,n3,n0,n2,fockb,b1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(s81(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,b1,d2,s81)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x3,s81, 1.000)
       deallocate(s81)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n2,n2,n3,fockb,b1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s82(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
       i1=k4
       i2=k2*k4*k4
       i3=k2
       call egemm(i1,i2,i3,b1,d2,s82)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n2,n3,n2,n3,n2,n3,n0,n2,x4,s82,-1.000)
       deallocate(s82)
c
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n0,n1,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u61(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u61)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x64(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       x64=0.0d0
       call sum345261(n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,x64,u61,1.000)
       deallocate(u61)
c
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder512346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(u62(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k2*k3*k3
       i3=k4*k1
       call egemm(i1,i2,i3,d1,f2,u62)
       deallocate(d1)
       deallocate(f2)
c
       call sum234561(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x29,u62,1.000)
       deallocate(u62)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(s83(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s83)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n0,n1,n2,n3,n0,n2,n0,n1,x1,s83, 1.000)
       deallocate(s83)
c
       allocate(d1(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u63(n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u63)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x65(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
       x65=0.0d0
       call sum356241(n0,n2,n1,n3,n2,n3,n1,n3,n0,n2,n0,n2,x65,u63,1.000)
       deallocate(u63)
c
       allocate(d1(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n1,n3,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder123456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
       allocate(u64(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k1*k1*k2*k3
       i3=k3*k4
       call egemm(i1,i2,i3,d1,f2,u64)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder651234(n1,n3,n0,n2,n0,n1,n0,n1,n1,n3,n0,n2,
!     & n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,u64,f1)
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z189(n2+1:n3,n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k3
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,f1,d2,z189)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     - u64(a,l,j,i,b,m)*t2c(d,c,m,k)          !aljibmdcmk      (-1.000)
     &     + u64(b,l,j,i,a,m)*t2c(d,c,m,k)          !bljiamdcmk      (+1.000)
     &     + u64(a,k,j,i,b,m)*t2c(d,c,m,l)          !akjibmdcml      (+1.000)
     &     - u64(b,k,j,i,a,m)*t2c(d,c,m,l)          !bkjiamdcml      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12634578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z189,-1.000)
!       call sum12643578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z189, 1.000)
!       call sum12534678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z189, 1.000)
!       call sum12543678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z189,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z189(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & +z189(d,c,k,a,b,l,j,i)       ! 12643578 (+1.000)
!     & +z189(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & -z189(d,c,l,a,b,k,j,i)       ! 12543678 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z189)
       deallocate(u64)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n1,n3,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(s84(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s84)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n1,n3,n2,n3,n1,n3,n0,n2,x2,s84, 1.000)
       deallocate(s84)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(u65(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k1*k3*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u65)
       deallocate(d1)
       deallocate(f2)
c
       call sum235614(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x62,u65,1.000)
       deallocate(u65)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s85(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s85)
       deallocate(d1)
       deallocate(d2)
c
       call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x36,s85, 1.000)
       deallocate(s85)
c
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder4312(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
       allocate(s86(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k4*k4
       i3=k2*k2
       call egemm(i1,i2,i3,d1,d2,s86)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n2,n3,n2,n3,n2,n3,n0,n2,x4,s86, 0.500)
       deallocate(s86)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u66(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u66)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x66(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       x66=0.0d0
       call sum456231(n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,x66,u66,1.000)
       deallocate(u66)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(s87(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k2*k2
       i3=k4*k4
       call egemm(i1,i2,i3,d1,d2,s87)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n0,n2,n0,n2,x3,s87, 0.500)
       deallocate(s87)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(s88(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s88)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n2,n3,n2,n3,n2,n3,n0,n2,x37,s88,-1.000)
       deallocate(s88)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u67(n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k3
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u67)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder546123(n2,n3,n0,n2,n0,n2,n1,n3,n0,n1,n0,n2,
!     & n0,n1,n1,n3,n0,n2,n2,n3,n0,n2,n0,n2,u67,f1)
!       allocate(h2(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       call reorder62513478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4b,h2)
!       allocate(z197(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k1*k1*k3*k3*k4
!       i3=k2*k3*k1
!       call egemm(i1,i2,i3,f1,h2,z197)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     + u67(d,l,k,e,m,n)*t4b(c,e,b,a,n,m,j,i)  !dlkemncebanmji  (+1.000)
     &     - u67(c,l,k,e,m,n)*t4b(d,e,b,a,n,m,j,i)  !clkemndebanmji  (-1.000)
             enddo;enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23478156(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z197, 1.000)
!       call sum13478256(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z197,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z197(c,b,a,j,i,d,l,k)       ! 23478156 (+1.000)
!     & -z197(d,b,a,j,i,c,l,k)       ! 13478256 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z197)
c
       allocate(f1(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder654123(n2,n3,n0,n2,n0,n2,n1,n3,n0,n1,n0,n2,
     & n0,n2,n0,n1,n1,n3,n2,n3,n0,n2,n0,n2,u67,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u149(n2+1:n3,n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u149)
       deallocate(f1)
       deallocate(b2)
c
       call sum312456(n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,
     & x63,u149,-1.000)
       deallocate(u149)
c
!       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
!       allocate(z182(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4*k4
!       i2=k1*k1*k3*k3
!       i3=k3*k1
!       call egemm(i1,i2,i3,x63,f2,z182)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + x63(m,e,d,c,l,k)*t3a(e,b,a,m,j,i)      !medclkebamji    (+1.000)
     &     - x63(m,e,c,d,l,k)*t3a(e,b,a,m,j,i)      !mecdlkebamji    (-1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34781256(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z182, 1.000)
!       call sum34782156(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z182,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z182(b,a,j,i,d,c,l,k)       ! 34781256 (+1.000)
!     & -z182(b,a,j,i,c,d,l,k)       ! 34782156 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z182)
       deallocate(x63)
c
       allocate(f1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder541236(n2,n3,n0,n2,n0,n2,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n2,n3,n0,n2,n0,n2,n0,n2,u67,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s155(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2*k4
       i3=k3*k1
       call egemm1(i1,i3,f1,b2,s155)
       deallocate(f1)
       deallocate(b2)
c
       call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x3,s155, 1.000)
       deallocate(s155)
c
       allocate(f1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder541236(n2,n3,n0,n2,n0,n2,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n2,n3,n0,n2,n0,n2,n0,n2,u67,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u116(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2*k4*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u116)
       deallocate(f1)
       deallocate(b2)
c
       call sum423561(n0,n2,n1,n3,n2,n3,n1,n3,n0,n2,n0,n2,
     & x65,u116,-1.000)
       deallocate(u116)
c
!       allocate(f2(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder421356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n1,n3,n2,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z188(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k3*k4
!       i2=k1*k1*k3*k4
!       i3=k3*k2
!       call egemm(i1,i2,i3,x65,f2,z188)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n2
             sum=sum
     &     + x65(m,e,d,b,l,k)*t3b(c,e,a,m,j,i)      !medblkceamji    (+1.000)
     &     - x65(m,e,d,a,l,k)*t3b(c,e,b,m,j,i)      !medalkcebmji    (-1.000)
     &     - x65(m,e,c,b,l,k)*t3b(d,e,a,m,j,i)      !mecblkdeamji    (-1.000)
     &     + x65(m,e,c,a,l,k)*t3b(d,e,b,m,j,i)      !mecalkdebmji    (+1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum24781356(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z188, 1.000)
!       call sum23781456(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z188,-1.000)
!       call sum14782356(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z188,-1.000)
!       call sum13782456(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z188, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z188(c,a,j,i,d,b,l,k)       ! 24781356 (+1.000)
!     & -z188(c,b,j,i,d,a,l,k)       ! 23781456 (-1.000)
!     & -z188(d,a,j,i,c,b,l,k)       ! 14782356 (-1.000)
!     & +z188(d,b,j,i,c,a,l,k)       ! 13782456 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z188)
       deallocate(x65)
c
       allocate(f1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder451236(n2,n3,n0,n2,n0,n2,n1,n3,n0,n1,n0,n2,
     & n1,n3,n0,n1,n2,n3,n0,n2,n0,n2,n0,n2,u67,f1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(u114(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2*k4*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,f1,b2,u114)
       deallocate(f1)
       deallocate(b2)
       deallocate(u67)
c
      call sum623451(n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,x64,u114,1.000)
c
       allocate(f1(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder623451(n0,n1,n0,n1,n2,n3,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,u114,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u182(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u182)
       deallocate(f1)
       deallocate(b2)
c
       call sum213456(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,
     & x30,u182,-1.000)
       deallocate(u182)
c
       allocate(f1(n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder263451(n0,n1,n0,n1,n2,n3,n0,n2,n0,n2,n0,n2,
     & n0,n1,n0,n2,n2,n3,n0,n2,n0,n2,n0,n1,u114,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u180(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2*k4*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u180)
       deallocate(f1)
       deallocate(b2)
       deallocate(u114)
c
      call sum312456(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x34,u180,1.000)
       deallocate(u180)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(h2(n2+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,
     & n0+1:n1,n0+1:n1))
       call reorder12634578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n1,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4b,h2)
       allocate(u68(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k1*k2*k3*k3
       i3=k1*k3*k4
       call egemm(i1,i2,i3,d1,h2,u68)
       deallocate(d1)
       deallocate(h2)
c
       call sum234561(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x62,u68,1.000)
       deallocate(u68)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(s89(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s89)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n2,n3,n0,n2,x9,s89, 1.000)
c
       allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4312(n2,n3,n0,n2,n1,n3,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,s89,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s157(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s157)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n1,n3,n2,n3,n1,n3,n0,n2,x2,s157,-1.000)
       deallocate(s157)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3412(n2,n3,n0,n2,n1,n3,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,s89,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s154(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s154)
       deallocate(d1)
       deallocate(b2)
       deallocate(s89)
c
       call sum4123(n0,n1,n2,n3,n0,n2,n0,n1,x1,s154, 1.000)
       deallocate(s154)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(u69(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u69)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder546123(n2,n3,n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n0,n2,n2,n3,n0,n2,n0,n2,u69,f1)
!       allocate(h2(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       call reorder61523478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n2,n2,n3,n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4c,h2)
!       allocate(z200(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k1*k1*k3*k3*k4
!       i3=k2*k4*k2
!       call egemm(i1,i2,i3,f1,h2,z200)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n2;do n=n0+1,n2
             sum=sum
     &     + (u69(d,l,k,f,m,n)*t4c(f,c,b,a,n,m,j,i)       !dlkfmnfcbanmji  (+0.500)
     &     - u69(c,l,k,f,m,n)*t4c(f,d,b,a,n,m,j,i))/2.0d0 !clkfmnfdbanmji  (-0.500)
             enddo;enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23478156(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z200, 0.500)
!       call sum13478256(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z200,-0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z200(c,b,a,j,i,d,l,k)      ! 23478156 (+0.500)
!     & -z200(d,b,a,j,i,c,l,k))/2.0d0! 13478256 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z200)
c
       allocate(f1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder541236(n2,n3,n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,u69,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s189(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2*k4
       i3=k4*k2
       call egemm1(i1,i3,f1,b2,s189)
       deallocate(f1)
       deallocate(b2)
c
       call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x3,s189,-1.000)
       deallocate(s189)
c
       allocate(f1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder541236(n2,n3,n0,n2,n0,n2,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,u69,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u151(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u151)
       deallocate(f1)
       deallocate(b2)
       deallocate(u69)
c
       call sum324561(n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,
     & x66,u151,-1.000)
       deallocate(u151)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z194(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4*k4
!       i2=k1*k1*k3*k3
!       i3=k4*k2
!       call egemm(i1,i2,i3,x66,f2,z194)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x66(m,f,d,c,l,k)*t3b(f,b,a,m,j,i)      !mfdclkfbamji    (+1.000)
     &     - x66(m,f,c,d,l,k)*t3b(f,b,a,m,j,i)      !mfcdlkfbamji    (-1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34781256(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z194, 1.000)
!       call sum34782156(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z194,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z194(b,a,j,i,d,c,l,k)       ! 34781256 (+1.000)
!     & -z194(b,a,j,i,c,d,l,k)       ! 34782156 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z194)
       deallocate(x66)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(s90(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k2
       i3=k4*k4
       call egemm(i1,i2,i3,d1,d2,s90)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x21(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       x21=0.0d0
       call sum3421(n0,n2,n0,n2,n0,n2,n0,n2,x21,s90, 0.500)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder3412(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s90,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(u174(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,d1,d2,u174)
       deallocate(d1)
       deallocate(d2)
c
      call sum236145(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x49,u174,0.500)
       deallocate(u174)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder3412(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s90,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s188(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s188)
       deallocate(d1)
       deallocate(b2)
       deallocate(s90)
c
       call sum2134(n0,n2,n2,n3,n0,n2,n0,n2,x3,s188,-0.500)
       deallocate(s188)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(h2(n2+1:n3,n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,
     & n0+1:n1,n0+1:n1))
       call reorder12534678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n2,n3,n2,n3,n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4c,h2)
       allocate(u70(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k1*k2*k3*k3
       i3=k2*k4*k4
       call egemm(i1,i2,i3,d1,h2,u70)
       deallocate(d1)
       deallocate(h2)
c
!       allocate(f1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder612345(n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n2,
!     & n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,u70,f1)
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z202(n2+1:n3,n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k3
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,f1,d2,z202)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum                                !top two switched
     &     + (u70(b,a,k,j,i,m)*t2c(d,c,m,l)         !bakjimdcml  (+0.500)
     &     - u70(b,a,l,j,i,m)*t2c(d,c,m,k))/2.0d0   !baljimdcmk  (-0.500)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12634578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z202,-0.500)
!       call sum12534678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z202, 0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z202(d,c,l,b,a,k,j,i)      ! 12534678 (+0.500) top two switched
!     & -z202(d,c,k,b,a,l,j,i))/2.0d0! 12634578 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z202)
       deallocate(u70)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(s91(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s91)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n2,n3,n0,n2,x22,s91, 1.000)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder4312(n2,n3,n0,n2,n2,n3,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,s91,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s190(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s190)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n2,n3,n0,n2,x37,s190,-1.000)
       deallocate(s190)
c
!       allocate(f2(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder123456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
!       allocate(z57(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k1*k1*k2*k3*k3
!       i3=k4
!       call egemm(i1,i2,i3,x37,f2,z57)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     + x37(e,d,c,k)*t3b(e,b,a,l,j,i)          !edckebalji      (+1.000)
     &     - x37(e,c,d,k)*t3b(e,b,a,l,j,i)          !ecdkebalji      (-1.000)
     &     - x37(e,d,c,l)*t3b(e,b,a,k,j,i)          !edclebakji      (-1.000)
     &     + x37(e,c,d,l)*t3b(e,b,a,k,j,i)          !ecdlebakji      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34578126(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z57, 1.000)
!       call sum34578216(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z57,-1.000)
!       call sum34678125(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z57,-1.000)
!       call sum34678215(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z57, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z57(b,a,l,j,i,d,c,k)       ! 34578126 (+1.000)
!     & -z57(b,a,l,j,i,c,d,k)       ! 34578216 (-1.000)
!     & -z57(b,a,k,j,i,d,c,l)       ! 34678125 (-1.000)
!     & +z57(b,a,k,j,i,c,d,l)       ! 34678215 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z57)
       deallocate(x37)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3412(n2,n3,n0,n2,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,s91,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s186(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s186)
       deallocate(d1)
       deallocate(b2)
       deallocate(s91)
c
       call sum4123(n0,n2,n2,n3,n0,n2,n0,n2,x36,s186,-1.000)
       deallocate(s186)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z56(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k1*k1*k3*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x36,f2,z56)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + x36(m,d,l,k)*t3b(c,b,a,m,j,i)          !mdlkcbamji      (+1.000)
     &     - x36(m,c,l,k)*t3b(d,b,a,m,j,i)          !mclkdbamji      (-1.000)
     &     - x36(m,d,k,l)*t3b(c,b,a,m,j,i)          !mdklcbamji      (-1.000)
     &     + x36(m,c,k,l)*t3b(d,b,a,m,j,i)          !mckldbamji      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23478156(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z56, 1.000)
!       call sum13478256(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z56,-1.000)
!       call sum23478165(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z56,-1.000)
!       call sum13478265(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z56, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z56(c,b,a,j,i,d,l,k)       ! 23478156 (+1.000)
!     & -z56(d,b,a,j,i,c,l,k)       ! 13478256 (-1.000)
!     & -z56(c,b,a,j,i,d,k,l)       ! 23478165 (-1.000)
!     & +z56(d,b,a,j,i,c,k,l)       ! 13478265 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z56)
       deallocate(x36)
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
       call sum21(n0,n2,n0,n2,x12,q19,-0.500)
       deallocate(q19)
c
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder4312(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
       allocate(s92(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k4*k4
       i3=k2*k2
       call egemm(i1,i2,i3,d1,d2,s92)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x23(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       x23=0.0d0
       call sum3412(n2,n3,n2,n3,n2,n3,n2,n3,x23,s92, 0.500)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder4312(n2,n3,n2,n3,n2,n3,n2,n3,
     & n2,n3,n2,n3,n2,n3,n2,n3,s92,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s187(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4*k4
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s187)
       deallocate(d1)
       deallocate(b2)
       deallocate(s92)
c
       call sum4123(n2,n3,n2,n3,n2,n3,n0,n2,x4,s187, 0.500)
       deallocate(s187)
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
       call sum21(n2,n3,n2,n3,x13,q20, 0.500)
       deallocate(q20)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(u71(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k1*k3*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u71)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x67(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       x67=0.0d0
       call sum345621(n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,x67,u71,1.000)
       deallocate(u71)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder124356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(s93(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k1*k3*k3
       call egemm(i1,i2,i3,d1,f2,s93)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x5,s93,-0.500)
       deallocate(s93)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n1,n3,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder154236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n1,n3,n1,n3,n0,n1,t3a,f2)
       allocate(s94(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k1*k1*k3
       call egemm(i1,i2,i3,d1,f2,s94)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x6,s94, 0.500)
       deallocate(s94)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(u72(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k1*k3*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u72)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x68(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       x68=0.0d0
       call sum345621(n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x68,u72,1.000)
c
       allocate(f1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder561234(n1,n3,n1,n3,n0,n1,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,u72,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u148(n0+1:n2,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u148)
       deallocate(f1)
       deallocate(b2)
       deallocate(u72)
c
      call sum412356(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x62,u148,1.000)
       deallocate(u148)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n1,n3,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder265134(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n2,n3,n1,n3,n0,n2,t3b,f2)
       allocate(s95(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
       i1=k3
       i2=k2*k3*k4
       i3=k1*k1*k3
       call egemm(i1,i2,i3,d1,f2,s95)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n1,n3,
!     & n1,n3,n2,n3,n1,n3,n0,n2,s95,d1)
!       allocate(f2(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder213456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
!       allocate(z211(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n1+1:n3,n0+1:n2))
!       i1=k2*k3*k4
!       i2=k1*k1*k2*k3*k4
!       i3=k3
!       call egemm(i1,i2,i3,d1,f2,z211)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3
             sum=sum
     &     + (s95(c,a,l,f)*t3b(d,f,b,k,j,i)         !calfdfbkji      (+0.500)
     &     - s95(c,b,l,f)*t3b(d,f,a,k,j,i)          !cblfdfakji      (-0.500)
     &     - s95(d,a,l,f)*t3b(c,f,b,k,j,i)          !dalfcfbkji      (-0.500)
     &     + s95(d,b,l,f)*t3b(c,f,a,k,j,i)          !dblfcfakji      (+0.500)
     &     - s95(c,a,k,f)*t3b(d,f,b,l,j,i)          !cakfdfblji      (-0.500)
     &     + s95(c,b,k,f)*t3b(d,f,a,l,j,i)          !cbkfdfalji      (+0.500)
     &     + s95(d,a,k,f)*t3b(c,f,b,l,j,i)          !dakfcfblji      (+0.500)
     &     - s95(d,b,k,f)*t3b(c,f,a,l,j,i))/2.0d0   !dbkfcfalji      (-0.500)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13678245(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z211, 0.500)
!       call sum14678235(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z211,-0.500)
!       call sum23678145(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z211,-0.500)
!       call sum24678135(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z211, 0.500)
!       call sum13578246(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z211,-0.500)
!       call sum14578236(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z211, 0.500)
!       call sum23578146(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z211, 0.500)
!       call sum24578136(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z211,-0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z211(d,b,k,j,i,c,a,l)      ! 13678245 (+0.500)
!     & -z211(d,a,k,j,i,c,b,l)       ! 14678235 (-0.500)
!     & -z211(c,b,k,j,i,d,a,l)       ! 23678145 (-0.500)
!     & +z211(c,a,k,j,i,d,b,l)       ! 24678135 (+0.500)
!     & -z211(d,b,l,j,i,c,a,k)       ! 13578246 (-0.500)
!     & +z211(d,a,l,j,i,c,b,k)       ! 14578236 (+0.500)
!     & +z211(c,b,l,j,i,d,a,k)       ! 23578146 (+0.500)
!     & -z211(c,a,l,j,i,d,b,k))/2.0d0! 24578136 (-0.500)
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
       deallocate(s95)
c
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder231456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n1,n3,n2,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
       allocate(u73(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1*k2*k4
       i3=k3*k3
       call egemm(i1,i2,i3,d1,f2,u73)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder651234(n2,n3,n0,n2,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,u73,f1)
!       allocate(f2(n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
!       call reorder561234(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,t3b,f2)
!       allocate(z212(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k4
!       i2=k2*k3*k3*k4
!       i3=k1*k1
!       call egemm(i1,i2,i3,f1,f2,z212)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum                                  !top two switched
     &     + (u73(c,k,j,i,m,n)*t3b(d,b,a,l,n,m)       !ckjimndbalnm    (+0.250)
     &     - u73(d,k,j,i,m,n)*t3b(c,b,a,l,n,m)        !dkjimncbalnm    (-0.250)
     &     + u73(d,l,j,i,m,n)*t3b(c,b,a,k,n,m)        !dljimncbaknm    (+0.250)
     &     - u73(c,l,j,i,m,n)*t3b(d,b,a,k,n,m))/4.0d0 !cljimndbaknm    (-0.250)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23451678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z212,-0.250)
!       call sum13452678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z212, 0.250)
!       call sum23461578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z212, 0.250)
!       call sum13462578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z212,-0.250)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z212(d,b,a,l,c,k,j,i)      ! 13452678 (+0.250) top two switched
!     & -z212(c,b,a,l,d,k,j,i)       ! 23451678 (-0.250)
!     & +z212(c,b,a,k,d,l,j,i)       ! 23461578 (+0.250)
!     & -z212(d,b,a,k,c,l,j,i))/4.0d0! 13462578 (-0.250)
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
c
       allocate(f1(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder561234(n2,n3,n0,n2,n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,u73,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u107(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k4*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u107)
       deallocate(f1)
       deallocate(b2)
       deallocate(u73)
c
!       allocate(f1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder231456(n1,n3,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,u107,f1)
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z295(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k4
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z295)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     + (u107(b,n,c,l,j,i)*t2b(d,a,k,n)        !bncljidakn      (+0.500)
     &     - u107(a,n,c,l,j,i)*t2b(d,b,k,n)         !ancljidbkn      (-0.500)
     &     - u107(b,n,d,l,j,i)*t2b(c,a,k,n)         !bndljicakn      (-0.500)
     &     + u107(a,n,d,l,j,i)*t2b(c,b,k,n)         !andljicbkn      (+0.500)
     &     - u107(b,n,c,k,j,i)*t2b(d,a,l,n)         !bnckjidaln      (-0.500)
     &     + u107(a,n,c,k,j,i)*t2b(d,b,l,n)         !anckjidbln      (+0.500)
     &     + u107(b,n,d,k,j,i)*t2b(c,a,l,n)         !bndkjicaln      (+0.500)
     &     - u107(a,n,d,k,j,i)*t2b(c,b,l,n))/2.0d0  !andkjicbln      (-0.500)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14623578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z295, 0.500)
!       call sum13624578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z295,-0.500)
!       call sum24613578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z295,-0.500)
!       call sum23614578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z295, 0.500)
!       call sum14523678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z295,-0.500)
!       call sum13524678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z295, 0.500)
!       call sum24513678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z295, 0.500)
!       call sum23514678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z295,-0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z295(d,a,k,c,b,l,j,i)      ! 14623578 (+0.500)
!     & -z295(d,b,k,c,a,l,j,i)       ! 13624578 (-0.500)
!     & -z295(c,a,k,d,b,l,j,i)       ! 24613578 (-0.500)
!     & +z295(c,b,k,d,a,l,j,i)       ! 23614578 (+0.500)
!     & -z295(d,a,l,c,b,k,j,i)       ! 14523678 (-0.500)
!     & +z295(d,b,l,c,a,k,j,i)       ! 13524678 (+0.500)
!     & +z295(c,a,l,d,b,k,j,i)       ! 24513678 (+0.500)
!     & -z295(c,b,l,d,a,k,j,i))/2.0d0! 23514678 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z295)
       deallocate(u107)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder235146(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n2,n3,n0,n2,n0,n1,t3b,f2)
       allocate(s96(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k2*k4
       i3=k1*k3*k3
       call egemm(i1,i2,i3,d1,f2,s96)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s96, 0.500)
       deallocate(s96)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder251346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(u74(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k2*k3*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u74)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder651234(n2,n3,n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,
!     & n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,u74,f1)
!       allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder521346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
!       allocate(z214(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k4
!       i2=k1*k2*k3*k4
!       i3=k3*k1
!       call egemm(i1,i2,i3,f1,f2,z214)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do n=n0+1,n1
             sum=sum
     &     + u74(c,a,l,j,f,n)*t3b(d,f,b,k,n,i)      !caljfndfbkni    (+1.000)
     &     - u74(d,a,k,i,f,n)*t3b(c,f,b,l,n,j)      !dakifncfblnj    (-1.000)
     &     - u74(d,a,l,j,f,n)*t3b(c,f,b,k,n,i)      !daljfncfbkni    (-1.000)
     &     + u74(c,a,k,i,f,n)*t3b(d,f,b,l,n,j)      !cakifndfblnj    (+1.000)
     &     - u74(c,a,k,j,f,n)*t3b(d,f,b,l,n,i)      !cakjfndfblni    (-1.000)
     &     + u74(d,a,l,i,f,n)*t3b(c,f,b,k,n,j)      !dalifncfbknj    (+1.000)
     &     + u74(d,a,k,j,f,n)*t3b(c,f,b,l,n,i)      !dakjfncfblni    (+1.000)
     &     - u74(c,a,l,i,f,n)*t3b(d,f,b,k,n,j)      !califndfbknj    (-1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13682457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z214, 1.000)
!       call sum23571468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z214,-1.000)
!       call sum23681457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z214,-1.000)
!       call sum13572468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z214, 1.000)
!       call sum13582467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z214,-1.000)
!       call sum23671458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z214, 1.000)
!       call sum23581467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z214, 1.000)
!       call sum13672458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z214,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z214(d,b,k,i,c,a,l,j)       ! 13682457 (+1.000)
!     & -z214(c,b,l,j,d,a,k,i)       ! 23571468 (-1.000)
!     & -z214(c,b,k,i,d,a,l,j)       ! 23681457 (-1.000)
!     & +z214(d,b,l,j,c,a,k,i)       ! 13572468 (+1.000)
!     & -z214(d,b,l,i,c,a,k,j)       ! 13582467 (-1.000)
!     & +z214(c,b,k,j,d,a,l,i)       ! 23671458 (+1.000)
!     & +z214(c,b,l,i,d,a,k,j)       ! 23581467 (+1.000)
!     & -z214(d,b,k,j,c,a,l,i)       ! 13672458 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z214)
c
       allocate(f1(n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder561234(n2,n3,n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,
     & n1,n3,n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,u74,f1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(u105(n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k4*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,f1,b2,u105)
       deallocate(f1)
       deallocate(b2)
       deallocate(u74)
c
      call sum612345(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x52,u105,1.000)
       deallocate(u105)
c
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n2,n1+1:n3))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n0,n2,n1,n3,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder164235(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n0,n1,n0,n2,n2,n3,n1,n3,n0,n2,t3c,f2)
       allocate(s97(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
       i1=k3
       i2=k2*k3*k4
       i3=k2*k1*k4
       call egemm(i1,i2,i3,d1,f2,s97)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x2,s97,-1.000)
       deallocate(s97)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n2,n2,n3,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder364125(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n1,n3,n0,n1,n0,n2,n2,n3,n2,n3,n0,n2,t3c,f2)
       allocate(s98(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
       i1=k4
       i2=k2*k4*k4
       i3=k2*k1*k3
       call egemm(i1,i2,i3,d1,f2,s98)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n2,n3,n0,n2,n2,n3,
!     & n2,n3,n2,n3,n2,n3,n0,n2,s98,d1)
!       allocate(f2(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder123456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
!       allocate(z216(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k1*k1*k2*k3*k3
!       i3=k4
!       call egemm(i1,i2,i3,d1,f2,z216)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum
     &     + s98(d,c,l,f)*t3b(f,b,a,k,j,i)          !dclffbakji      (+1.000)
     &     - s98(d,c,k,f)*t3b(f,b,a,l,j,i)          !dckffbalji      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34678125(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z216, 1.000)
!       call sum34578126(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z216,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z216(b,a,k,j,i,d,c,l)       ! 34678125 (+1.000)
!     & -z216(b,a,l,j,i,d,c,k)       ! 34578126 (-1.000)
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
       deallocate(s98)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder123456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
       allocate(u75(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k1*k2*k3
       i3=k3*k4
       call egemm(i1,i2,i3,d1,f2,u75)
       deallocate(d1)
       deallocate(f2)
c
      call sum345621(n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,x43,u75, 1.000)
c
!       allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder461235(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,t3c,f2)
!       allocate(z91(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3
!       i2=k2*k3*k4*k4
!       i3=k1*k2
!       call egemm(i1,i2,i3,x43,f2,z91)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     + x43(n,m,b,k,j,i)*t3c(d,c,a,n,l,m)      !nmbkjidcanlm    (+1.000)
     &     - x43(n,m,a,k,j,i)*t3c(d,c,b,n,l,m)      !nmakjidcbnlm    (-1.000)
     &     - x43(n,m,b,l,j,i)*t3c(d,c,a,n,k,m)      !nmbljidcankm    (-1.000)
     &     + x43(n,m,a,l,j,i)*t3c(d,c,b,n,k,m)      !nmaljidcbnkm    (+1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12453678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z91, 1.000)
!       call sum12354678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z91,-1.000)
!       call sum12463578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z91,-1.000)
!       call sum12364578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z91, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z91(d,c,a,l,b,k,j,i)       ! 12453678 (+1.000)
!     & -z91(d,c,b,l,a,k,j,i)       ! 12354678 (-1.000)
!     & -z91(d,c,a,k,b,l,j,i)       ! 12463578 (-1.000)
!     & +z91(d,c,b,k,a,l,j,i)       ! 12364578 (+1.000)
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
       deallocate(x43)
c
       allocate(f1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder651234(n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,n0,n2,
     & n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,u75,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u143(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u143)
       deallocate(f1)
       deallocate(b2)
c
      call sum213456(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x39,u143,1.000)
       deallocate(u143)
c
       allocate(f1(n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder561234(n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n1,n3,n0,n2,n0,n1,n0,n1,u75,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u117(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k3*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u117)
       deallocate(f1)
       deallocate(b2)
       deallocate(u75)
c
!!!!!!!!! sign changed explicitly by nicholsa p. bauman to have same
contribution as its compliment at line 9536.
       call sum213456(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x31,u117, 1.000)
       deallocate(u117)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z266(n2+1:n3,n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k3
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x31,d2,z266)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - x31(n,b,a,k,j,i)*t2c(d,c,n,l)          !nbakjidcnl      (-1.000)
     &     + x31(n,a,b,k,j,i)*t2c(d,c,n,l)          !nabkjidcnl      (+1.000)
     &     + x31(n,b,a,l,j,i)*t2c(d,c,n,k)          !nbaljidcnk      (+1.000)
     &     - x31(n,a,b,l,j,i)*t2c(d,c,n,k)          !nabljidcnk      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12534678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z266,-1.000)
!       call sum12543678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z266, 1.000)
!       call sum12634578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z266, 1.000)
!       call sum12643578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z266,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z266(d,c,l,b,a,k,j,i)       ! 12534678 (-1.000)
!     & +z266(d,c,l,a,b,k,j,i)       ! 12543678 (+1.000)
!     & +z266(d,c,k,b,a,l,j,i)       ! 12634578 (+1.000)
!     & -z266(d,c,k,a,b,l,j,i)       ! 12643578 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z266)
       deallocate(x31)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder136245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n1,n2,n3,n0,n2,n0,n2,t3c,f2)
       allocate(s99(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2*k2*k4
       i3=k1*k3*k4
       call egemm(i1,i2,i3,d1,f2,s99)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder4123(n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n0,n2,n0,n2,s99,d1)
!       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z218(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k1*k1*k3*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,d1,f2,z218)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - s99(c,l,k,n)*t3b(d,b,a,n,j,i)          !clkndbanji      (-1.000)
     &     + s99(d,l,k,n)*t3b(c,b,a,n,j,i)          !dlkncbanji      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13478256(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z218,-1.000)
!       call sum23478156(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z218, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z218(d,b,a,j,i,c,l,k)       ! 13478256 (-1.000)
!     & +z218(c,b,a,j,i,d,l,k)       ! 23478156 (+1.000)
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
       deallocate(s99)
c
       allocate(d1(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n2,n3,n0,n1,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder241356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n2,n2,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(u76(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4
       i2=k1*k1*k3*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,f2,u76)
       deallocate(d1)
       deallocate(f2)
c
      call sum345621(n0,n1,n2,n3,n2,n3,n1,n3,n0,n1,n0,n1,x44,u76,-1.000)
c
!       allocate(f2(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
!       call reorder612345(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,t3c,f2)
!       allocate(z92(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k4
!       i2=k2*k2*k3*k4
!       i3=k4*k1
!       call egemm(i1,i2,i3,x44,f2,z92)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n1
             sum=sum
     &     + x44(m,f,d,b,j,i)*t3c(f,c,a,l,k,m)      !mfdbjifcalkm    (+1.000)
     &     - x44(m,f,d,a,j,i)*t3c(f,c,b,l,k,m)      !mfdajifcblkm    (-1.000)
     &     - x44(m,f,c,b,j,i)*t3c(f,d,a,l,k,m)      !mfcbjifdalkm    (-1.000)
     &     + x44(m,f,c,a,j,i)*t3c(f,d,b,l,k,m)      !mfcajifdblkm    (+1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum24561378(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z92, 1.000)
!       call sum23561478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z92,-1.000)
!       call sum14562378(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z92,-1.000)
!       call sum13562478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z92, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z92(c,a,l,k,d,b,j,i)       ! 24561378 (+1.000)
!     & -z92(c,b,l,k,d,a,j,i)       ! 23561478 (-1.000)
!     & -z92(d,a,l,k,c,b,j,i)       ! 14562378 (-1.000)
!     & +z92(d,b,l,k,c,a,j,i)       ! 13562478 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z92)
       deallocate(x44)
c
       allocate(f1(n2+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder561234(n2,n3,n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n1,n3,n0,n1,n0,n1,u76,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u142(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k4*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u142)
       deallocate(f1)
       deallocate(b2)
       deallocate(u76)
c
      call sum412356(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x39,u142,1.000)
       deallocate(u142)
c
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z332(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k4
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x39,d2,z332)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x39(m,d,b,k,j,i)*t2b(c,a,l,m)          !mdbkjicalm      (+1.000)
     &     - x39(m,d,a,k,j,i)*t2b(c,b,l,m)          !mdakjicblm      (-1.000)
     &     - x39(m,c,b,k,j,i)*t2b(d,a,l,m)          !mcbkjidalm      (-1.000)
     &     + x39(m,c,a,k,j,i)*t2b(d,b,l,m)          !mcakjidblm      (+1.000)
     &     - x39(m,d,b,l,j,i)*t2b(c,a,k,m)          !mdbljicakm      (-1.000)
     &     + x39(m,d,a,l,j,i)*t2b(c,b,k,m)          !mdaljicbkm      (+1.000)
     &     + x39(m,c,b,l,j,i)*t2b(d,a,k,m)          !mcbljidakm      (+1.000)
     &     - x39(m,c,a,l,j,i)*t2b(d,b,k,m)          !mcaljidbkm      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum24513678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z332, 1.000)
!       call sum23514678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z332,-1.000)
!       call sum14523678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z332,-1.000)
!       call sum13524678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z332, 1.000)
!       call sum24613578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z332,-1.000)
!       call sum23614578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z332, 1.000)
!       call sum14623578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z332, 1.000)
!       call sum13624578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z332,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z332(c,a,l,d,b,k,j,i)       ! 24513678 (+1.000)
!     & -z332(c,b,l,d,a,k,j,i)       ! 23514678 (-1.000)
!     & -z332(d,a,l,c,b,k,j,i)       ! 14523678 (-1.000)
!     & +z332(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & -z332(c,a,k,d,b,l,j,i)       ! 24613578 (-1.000)
!     & +z332(c,b,k,d,a,l,j,i)       ! 23614578 (+1.000)
!     & +z332(d,a,k,c,b,l,j,i)       ! 14623578 (+1.000)
!     & -z332(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z332)
       deallocate(x39)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(u77(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k1*k3*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u77)
       deallocate(d1)
       deallocate(f2)
c
       call sum345621(n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,x67,u77,1.000)
       deallocate(u77)
c
!       allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder631245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
!       allocate(z207(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3
!       i2=k2*k2*k4*k4
!       i3=k3*k1
!       call egemm(i1,i2,i3,x67,f2,z207)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do n=n0+1,n1
             sum=sum
     &     + x67(n,f,b,a,j,i)*t3c(d,c,f,l,k,n)      !nfbajidcflkn    (+1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12563478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z207, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z207(d,c,l,k,b,a,j,i)       ! 12563478 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z207)
       deallocate(x67)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder124356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n1,n3,n0,n2,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(s100(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k2*k3*k4
       call egemm(i1,i2,i3,d1,f2,s100)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x5,s100, 1.000)
       deallocate(s100)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder134256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n2,n3,n0,n2,n0,n1,t3c,f2)
       allocate(s101(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k2*k4
       i3=k2*k3*k4
       call egemm(i1,i2,i3,d1,f2,s101)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s101, 1.000)
       deallocate(s101)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder251346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(u78(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k2*k3*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u78)
       deallocate(d1)
       deallocate(f2)
c
       call sum345621(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x57,u78,1.000)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
!       allocate(z149(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k4
!       i2=k1*k2*k3*k4
!       i3=k4*k2
!       call egemm(i1,i2,i3,x57,f2,z149)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x57(m,f,d,b,k,i)*t3c(f,c,a,m,l,j)      !mfdbkifcamlj    (+1.000)
     &     - x57(m,f,d,a,k,i)*t3c(f,c,b,m,l,j)      !mfdakifcbmlj    (-1.000)
     &     - x57(m,f,c,b,k,i)*t3c(f,d,a,m,l,j)      !mfcbkifdamlj    (-1.000)
     &     + x57(m,f,c,a,k,i)*t3c(f,d,b,m,l,j)      !mfcakifdbmlj    (+1.000)
     &     - x57(m,f,d,b,l,i)*t3c(f,c,a,m,k,j)      !mfdblifcamkj    (-1.000)
     &     + x57(m,f,d,a,l,i)*t3c(f,c,b,m,k,j)      !mfdalifcbmkj    (+1.000)
     &     + x57(m,f,c,b,l,i)*t3c(f,d,a,m,k,j)      !mfcblifdamkj    (+1.000)
     &     - x57(m,f,c,a,l,i)*t3c(f,d,b,m,k,j)      !mfcalifdbmkj    (-1.000)
     &     - x57(m,f,d,b,k,j)*t3c(f,c,a,m,l,i)      !mfdbkjfcamli    (-1.000)
     &     + x57(m,f,d,a,k,j)*t3c(f,c,b,m,l,i)      !mfdakjfcbmli    (+1.000)
     &     + x57(m,f,c,b,k,j)*t3c(f,d,a,m,l,i)      !mfcbkjfdamli    (+1.000)
     &     - x57(m,f,c,a,k,j)*t3c(f,d,b,m,l,i)      !mfcakjfdbmli    (-1.000)
     &     + x57(m,f,d,b,l,j)*t3c(f,c,a,m,k,i)      !mfdbljfcamki    (+1.000)
     &     - x57(m,f,d,a,l,j)*t3c(f,c,b,m,k,i)      !mfdaljfcbmki    (-1.000)
     &     - x57(m,f,c,b,l,j)*t3c(f,d,a,m,k,i)      !mfcbljfdamki    (-1.000)
     &     + x57(m,f,c,a,l,j)*t3c(f,d,b,m,k,i)      !mfcaljfdbmki    (+1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum24571368(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149, 1.000)
!       call sum23571468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149,-1.000)
!       call sum14572368(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149,-1.000)
!       call sum13572468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149, 1.000)
!       call sum24671358(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149,-1.000)
!       call sum23671458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149, 1.000)
!       call sum14672358(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149, 1.000)
!       call sum13672458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149,-1.000)
!       call sum24581367(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149,-1.000)
!       call sum23581467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149, 1.000)
!       call sum14582367(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149, 1.000)
!       call sum13582467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149,-1.000)
!       call sum24681357(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149, 1.000)
!       call sum23681457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149,-1.000)
!       call sum14682357(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149,-1.000)
!       call sum13682457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z149, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z149(c,a,l,j,d,b,k,i)       ! 24571368 (+1.000)
!     & -z149(c,b,l,j,d,a,k,i)       ! 23571468 (-1.000)
!     & -z149(d,a,l,j,c,b,k,i)       ! 14572368 (-1.000)
!     & +z149(d,b,l,j,c,a,k,i)       ! 13572468 (+1.000)
!     & -z149(c,a,k,j,d,b,l,i)       ! 24671358 (-1.000)
!     & +z149(c,b,k,j,d,a,l,i)       ! 23671458 (+1.000)
!     & +z149(d,a,k,j,c,b,l,i)       ! 14672358 (+1.000)
!     & -z149(d,b,k,j,c,a,l,i)       ! 13672458 (-1.000)
!     & -z149(c,a,l,i,d,b,k,j)       ! 24581367 (-1.000)
!     & +z149(c,b,l,i,d,a,k,j)       ! 23581467 (+1.000)
!     & +z149(d,a,l,i,c,b,k,j)       ! 14582367 (+1.000)
!     & -z149(d,b,l,i,c,a,k,j)       ! 13582467 (-1.000)
!     & +z149(c,a,k,i,d,b,l,j)       ! 24681357 (+1.000)
!     & -z149(c,b,k,i,d,a,l,j)       ! 23681457 (-1.000)
!     & -z149(d,a,k,i,c,b,l,j)       ! 14682357 (-1.000)
!     & +z149(d,b,k,i,c,a,l,j)       ! 13682457 (+1.000)
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
       deallocate(x57)
c
       allocate(f1(n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder561234(n2,n3,n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,u78,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u139(n0+1:n2,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u139)
       deallocate(f1)
       deallocate(b2)
       deallocate(u78)
c
       call sum512346(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & x28,u139,-1.000)
       deallocate(u139)
c
       allocate(d1(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n1,n3,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder152346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n0,n1,n1,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(u79(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k1*k2*k3*k3
       i3=k1*k4
       call egemm(i1,i2,i3,d1,f2,u79)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n2,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder651234(n1,n3,n1,n3,n0,n2,n0,n1,n1,n3,n0,n2,
!     & n0,n2,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,u79,f1)
!       allocate(f2(n0+1:n2,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder431256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n1,n3,n2,n3,n2,n3,n0,n2,n0,n1,t3c,f2)
!       allocate(z224(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k3
!       i2=k1*k2*k4*k4
!       i3=k3*k2
!       call egemm(i1,i2,i3,f1,f2,z224)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do n=n0+1,n2
             sum=sum
     &     - u79(b,a,k,i,e,n)*t3c(d,c,e,n,l,j)      !bakiendcenlj    (-1.000)
     &     + u79(b,a,l,i,e,n)*t3c(d,c,e,n,k,j)      !baliendcenkj    (+1.000)
     &     + u79(b,a,k,j,e,n)*t3c(d,c,e,n,l,i)      !bakjendcenli    (+1.000)
     &     - u79(b,a,l,j,e,n)*t3c(d,c,e,n,k,i)      !baljendcenki    (-1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12573468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z224,-1.000)
!       call sum12673458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z224, 1.000)
!       call sum12583467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z224, 1.000)
!       call sum12683457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z224,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z224(d,c,l,j,b,a,k,i)       ! 12573468 (-1.000)
!     & +z224(d,c,k,j,b,a,l,i)       ! 12673458 (+1.000)
!     & +z224(d,c,l,i,b,a,k,j)       ! 12583467 (+1.000)
!     & -z224(d,c,k,i,b,a,l,j)       ! 12683457 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z224)
c
       allocate(f1(n1+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder561234(n1,n3,n1,n3,n0,n2,n0,n1,n1,n3,n0,n2,
     & n1,n3,n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,u79,f1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(u115(n0+1:n1,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k3*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,f1,b2,u115)
       deallocate(f1)
       deallocate(b2)
       deallocate(u79)
c
      call sum612345(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x29,u115,1.000)
       deallocate(u115)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder125346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n1,n3,n0,n1,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(s102(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k2*k3
       i3=k1*k3*k4
       call egemm(i1,i2,i3,d1,f2,s102)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x7,s102, 1.000)
       deallocate(s102)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder132456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n1,n3,n2,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
       allocate(u80(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k2*k2*k4
       i3=k3*k4
       call egemm(i1,i2,i3,d1,f2,u80)
       deallocate(d1)
       deallocate(f2)
c
       call sum345621(n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,x64,u80,1.000)
c
!       allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder451236(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n0,n1,n2,n3,n1,n3,n1,n3,n0,n1,t3b,f2)
!       allocate(z185(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k4
!       i2=k1*k3*k3*k4
!       i3=k1*k2
!       call egemm(i1,i2,i3,x64,f2,z185)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     + x64(n,m,d,l,k,i)*t3b(c,b,a,n,m,j)      !nmdlkicbanmj    (+1.000)
     &     - x64(n,m,c,l,k,i)*t3b(d,b,a,n,m,j)      !nmclkidbanmj    (-1.000)
     &     - x64(n,m,d,l,k,j)*t3b(c,b,a,n,m,i)      !nmdlkjcbanmi    (-1.000)
     &     + x64(n,m,c,l,k,j)*t3b(d,b,a,n,m,i)      !nmclkjdbanmi    (+1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23471568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z185, 1.000)
!       call sum13472568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z185,-1.000)
!       call sum23481567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z185,-1.000)
!       call sum13482567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z185, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z185(c,b,a,j,d,l,k,i)       ! 23471568 (+1.000)
!     & -z185(d,b,a,j,c,l,k,i)       ! 13472568 (-1.000)
!     & -z185(c,b,a,i,d,l,k,j)       ! 23481567 (-1.000)
!     & +z185(d,b,a,i,c,l,k,j)       ! 13482567 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z185)
       deallocate(x64)
c
       allocate(f1(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder651234(n2,n3,n0,n2,n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,n0,n1,u80,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u129(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u129)
       deallocate(f1)
       deallocate(b2)
c
       call sum213456(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,
     & x30,u129,-1.000)
       deallocate(u129)
c
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z265(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k4*k4
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x30,d2,z265)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x30(m,d,c,l,k,i)*t2a(b,a,m,j)          !mdclkibamj      (+1.000)
     &     - x30(m,c,d,l,k,i)*t2a(b,a,m,j)          !mcdlkibamj      (-1.000)
     &     - x30(m,d,c,l,k,j)*t2a(b,a,m,i)          !mdclkjbami      (-1.000)
     &     + x30(m,c,d,l,k,j)*t2a(b,a,m,i)          !mcdlkjbami      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34712568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z265, 1.000)
!       call sum34721568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z265,-1.000)
!       call sum34812567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z265,-1.000)
!       call sum34821567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z265, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z265(b,a,j,d,c,l,k,i)       ! 34712568 (+1.000)
!     & -z265(b,a,j,c,d,l,k,i)       ! 34721568 (-1.000)
!     & -z265(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & +z265(b,a,i,c,d,l,k,j)       ! 34821567 (+1.000)
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
       allocate(f1(n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder561234(n2,n3,n0,n2,n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n2,n3,n0,n2,n0,n2,n0,n1,u80,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u113(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2*k4*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u113)
       deallocate(f1)
       deallocate(b2)
       deallocate(u80)
c
      call sum312456(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x34,u113,1.000)
       deallocate(u113)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n2,n2,n3,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder254136(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n2,n2,n3,n1,n3,n0,n1,t3b,f2)
       allocate(s103(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k3*k4
       i3=k2*k1*k3
       call egemm(i1,i2,i3,d1,f2,s103)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n2,n3,n2,n3,n1,n3,n0,n1,x8,s103,-1.000)
       deallocate(s103)
c
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n2,n1+1:n3))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n0,n2,n1,n3,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n1,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder154236(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n0,n1,n0,n2,n1,n3,n1,n3,n0,n1,t3b,f2)
       allocate(s104(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k2*k1*k4
       call egemm(i1,i2,i3,d1,f2,s104)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x6,s104,-1.000)
       deallocate(s104)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n2,n3,intb,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder154236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n2,n3,n0,n2,n0,n2,n2,n3,n2,n3,n0,n2,t3d,f2)
       allocate(s105(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
       i1=k4
       i2=k2*k4*k4
       i3=k2*k2*k4
       call egemm(i1,i2,i3,d1,f2,s105)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n2,n3,n2,n3,n2,n3,n0,n2,x4,s105,-0.500)
       deallocate(s105)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder124356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n2,n3,n0,n2,n0,n2,t3d,f2)
       allocate(s106(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2*k2*k4
       i3=k2*k4*k4
       call egemm(i1,i2,i3,d1,f2,s106)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x3,s106, 0.500)
       deallocate(s106)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(u81(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k1*k3*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u81)
       deallocate(d1)
       deallocate(f2)
c
       call sum345621(n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x68,u81,1.000)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder412356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
!       allocate(z210(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3
!       i2=k2*k2*k4*k4
!       i3=k4*k2
!       call egemm(i1,i2,i3,x68,f2,z210)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do n=n0+1,n2
             sum=sum
     &     + x68(n,f,b,a,j,i)*t3d(f,d,c,n,l,k)      !nfbajifdcnlk    (+1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12563478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z210, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z210(d,c,l,k,b,a,j,i)       ! 12563478 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z210)
       deallocate(x68)
c
       allocate(f1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder561234(n1,n3,n1,n3,n0,n1,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,u81,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u150(n0+1:n2,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u150)
       deallocate(f1)
       deallocate(b2)
       deallocate(u81)
c
      call sum412356(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x62,u150,1.000)
       deallocate(u150)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z181(n2+1:n3,n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k3
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x62,d2,z181)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     + x62(n,b,a,k,j,i)*t2c(d,c,n,l)          !nbakjidcnl      (+1.000)
     &     - x62(n,b,a,l,j,i)*t2c(d,c,n,k)          !nbaljidcnk      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12534678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z181, 1.000)
!       call sum12634578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z181,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z181(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & -z181(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
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
       deallocate(x62)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n2,n3,intb,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder154236(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n0,n2,n0,n2,n2,n3,n1,n3,n0,n1,t3c,f2)
       allocate(s107(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k3*k4
       i3=k2*k2*k4
       call egemm(i1,i2,i3,d1,f2,s107)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder4123(n2,n3,n1,n3,n0,n1,n2,n3,
!     & n2,n3,n2,n3,n1,n3,n0,n1,s107,d1)
!       allocate(f2(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder123456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
!       allocate(z232(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3*k4
!       i2=k1*k2*k2*k3*k4
!       i3=k4
!       call egemm(i1,i2,i3,d1,f2,z232)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum
     &     + (s107(c,a,j,f)*t3c(f,d,b,l,k,i)        !cajffdblki      (+0.500)
     &     - s107(c,b,j,f)*t3c(f,d,a,l,k,i)         !cbjffdalki      (-0.500)
     &     - s107(d,a,j,f)*t3c(f,c,b,l,k,i)         !dajffcblki      (-0.500)
     &     + s107(d,b,j,f)*t3c(f,c,a,l,k,i)         !dbjffcalki      (+0.500)
     &     - s107(d,b,i,f)*t3c(f,c,a,l,k,j)         !dbiffcalkj      (-0.500)
     &     + s107(d,a,i,f)*t3c(f,c,b,l,k,j)         !daiffcblkj      (+0.500)
     &     + s107(c,b,i,f)*t3c(f,d,a,l,k,j)         !cbiffdalkj      (+0.500)
     &     - s107(c,a,i,f)*t3c(f,d,b,l,k,j))/2.0d0  !caiffdblkj      (-0.500)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13568247(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z232, 0.500)
!       call sum14568237(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z232,-0.500)
!       call sum23568147(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z232,-0.500)
!       call sum24568137(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z232, 0.500)
!       call sum24567138(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z232,-0.500)
!       call sum23567148(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z232, 0.500)
!       call sum14567238(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z232, 0.500)
!       call sum13567248(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z232,-0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z232(d,b,l,k,i,c,a,j)      ! 13568247 (+0.500)
!     & -z232(d,a,l,k,i,c,b,j)       ! 14568237 (-0.500)
!     & -z232(c,b,l,k,i,d,a,j)       ! 23568147 (-0.500)
!     & +z232(c,a,l,k,i,d,b,j)       ! 24568137 (+0.500)
!     & -z232(c,a,l,k,j,d,b,i)       ! 24567138 (-0.500)
!     & +z232(c,b,l,k,j,d,a,i)       ! 23567148 (+0.500)
!     & +z232(d,a,l,k,j,c,b,i)       ! 14567238 (+0.500)
!     & -z232(d,b,l,k,j,c,a,i))/2.0d0! 13567248 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z232)
       deallocate(s107)
c
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder123456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
       allocate(u82(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k2*k2*k3
       i3=k4*k4
       call egemm(i1,i2,i3,d1,f2,u82)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder651234(n1,n3,n0,n2,n0,n2,n0,n1,n0,n2,n0,n2,
!     & n0,n2,n0,n2,n1,n3,n0,n2,n0,n2,n0,n1,u82,f1)
!       allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder451236(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n0,n2,n2,n3,n2,n3,n1,n3,n0,n1,t3c,f2)
!       allocate(z233(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3
!       i2=k1*k3*k4*k4
!       i3=k2*k2
!       call egemm(i1,i2,i3,f1,f2,z233)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2;do n=n0+1,n2
             sum=sum                                  !top two switched
     &     + (u82(a,l,k,i,m,n)*t3c(d,c,b,n,m,j)       !alkimndcbnmj    (+0.250)
     &     - u82(b,l,k,i,m,n)*t3c(d,c,a,n,m,j)        !blkimndcanmj    (-0.250)
     &     - u82(a,l,k,j,m,n)*t3c(d,c,b,n,m,i)        !alkjmndcbnmi    (-0.250)
     &     + u82(b,l,k,j,m,n)*t3c(d,c,a,n,m,i))/4.0d0 !blkjmndcanmi    (+0.250)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12473568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z233,-0.250)
!       call sum12374568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z233, 0.250)
!       call sum12384567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z233,-0.250)
!       call sum12483567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z233, 0.250)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z233(d,c,b,j,a,l,k,i)      ! 12374568 (+0.250) top two switched
!     & -z233(d,c,a,j,b,l,k,i)       ! 12473568 (-0.250)
!     & -z233(d,c,b,i,a,l,k,j)       ! 12384567 (-0.250)
!     & +z233(d,c,a,i,b,l,k,j))/4.0d0! 12483567 (+0.250)
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
c
       allocate(f1(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder561234(n1,n3,n0,n2,n0,n2,n0,n1,n0,n2,n0,n2,
     & n0,n2,n0,n2,n1,n3,n0,n2,n0,n2,n0,n1,u82,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u147(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2*k3*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u147)
       deallocate(f1)
       deallocate(b2)
       deallocate(u82)
c
      call sum213456(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x34,u147,0.500)
       deallocate(u147)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder124356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n2,n3,n0,n2,n1,n3,n0,n2,n0,n1,t3c,f2)
       allocate(s108(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k2*k3
       i3=k2*k4*k4
       call egemm(i1,i2,i3,d1,f2,s108)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder4123(n1,n3,n0,n2,n0,n1,n0,n2,
!     & n0,n2,n1,n3,n0,n2,n0,n1,s108,d1)
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
!       allocate(z234(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3
!       i2=k1*k2*k3*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,d1,f2,z234)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum                                !top two switched
     &     + (s108(b,l,j,n)*t3c(d,c,a,n,k,i)        !bljndcanki      (+0.500)
     &     - s108(a,l,j,n)*t3c(d,c,b,n,k,i)         !aljndcbnki      (-0.500)
     &     + s108(b,k,i,n)*t3c(d,c,a,n,l,j)         !bkindcanlj      (+0.500)
     &     - s108(a,k,i,n)*t3c(d,c,b,n,l,j)         !akindcbnlj      (-0.500)
     &     + s108(a,k,j,n)*t3c(d,c,b,n,l,i)         !akjndcbnli      (+0.500)
     &     - s108(b,k,j,n)*t3c(d,c,a,n,l,i)         !bkjndcanli      (-0.500)
     &     - s108(b,l,i,n)*t3c(d,c,a,n,k,j)         !blindcankj      (-0.500)
     &     + s108(a,l,i,n)*t3c(d,c,b,n,k,j))/2.0d0  !alindcbnkj      (+0.500)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12368457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z234,-0.500)
!       call sum12468357(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z234, 0.500)
!       call sum12457368(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z234, 0.500)
!       call sum12357468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z234,-0.500)
!       call sum12358467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z234, 0.500)
!       call sum12458367(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z234,-0.500)
!       call sum12467358(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z234,-0.500)
!       call sum12367458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z234, 0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z234(d,c,a,k,i,b,l,j)      ! 12468357 (+0.500) top two switched
!     & -z234(d,c,b,k,i,a,l,j)       ! 12368457 (-0.500)
!     & +z234(d,c,a,l,j,b,k,i)       ! 12457368 (+0.500)
!     & -z234(d,c,b,l,j,a,k,i)       ! 12357468 (-0.500)
!     & +z234(d,c,b,l,i,a,k,j)       ! 12358467 (+0.500)
!     & -z234(d,c,a,l,i,b,k,j)       ! 12458367 (-0.500)
!     & -z234(d,c,a,k,j,b,l,i)       ! 12467358 (-0.500)
!     & +z234(d,c,b,k,j,a,l,i))/2.0d0! 12367458 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z234)
       deallocate(s108)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder142356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
       allocate(u83(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k2*k3*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u83)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder651234(n2,n3,n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,u83,f1)
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
!       allocate(z235(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k4
!       i2=k1*k2*k3*k4
!       i3=k4*k2
!       call egemm(i1,i2,i3,f1,f2,z235)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do n=n0+1,n2
             sum=sum
     &     + u83(c,a,l,j,f,n)*t3c(f,d,b,n,k,i)      !caljfnfdbnki    (+1.000)
     &     - u83(d,a,k,i,f,n)*t3c(f,c,b,n,l,j)      !dakifnfcbnlj    (-1.000)
     &     - u83(d,a,l,j,f,n)*t3c(f,c,b,n,k,i)      !daljfnfcbnki    (-1.000)
     &     + u83(c,a,k,i,f,n)*t3c(f,d,b,n,l,j)      !cakifnfdbnlj    (+1.000)
     &     - u83(c,a,k,j,f,n)*t3c(f,d,b,n,l,i)      !cakjfnfdbnli    (-1.000)
     &     + u83(d,a,l,i,f,n)*t3c(f,c,b,n,k,j)      !dalifnfcbnkj    (+1.000)
     &     + u83(d,a,k,j,f,n)*t3c(f,c,b,n,l,i)      !dakjfnfcbnli    (+1.000)
     &     - u83(c,a,l,i,f,n)*t3c(f,d,b,n,k,j)      !califnfdbnkj    (-1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13682457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z235, 1.000)
!       call sum23571468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z235,-1.000)
!       call sum23681457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z235,-1.000)
!       call sum13572468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z235, 1.000)
!       call sum13582467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z235,-1.000)
!       call sum23671458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z235, 1.000)
!       call sum23581467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z235, 1.000)
!       call sum13672458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z235,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z235(d,b,k,i,c,a,l,j)       ! 13682457 (+1.000)
!     & -z235(c,b,l,j,d,a,k,i)       ! 23571468 (-1.000)
!     & -z235(c,b,k,i,d,a,l,j)       ! 23681457 (-1.000)
!     & +z235(d,b,l,j,c,a,k,i)       ! 13572468 (+1.000)
!     & -z235(d,b,l,i,c,a,k,j)       ! 13582467 (-1.000)
!     & +z235(c,b,k,j,d,a,l,i)       ! 23671458 (+1.000)
!     & +z235(c,b,l,i,d,a,k,j)       ! 23581467 (+1.000)
!     & -z235(d,b,k,j,c,a,l,i)       ! 13672458 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z235)
c
       allocate(f1(n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder561234(n2,n3,n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,u83,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u145(n0+1:n2,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u145)
       deallocate(f1)
       deallocate(b2)
       deallocate(u83)
c
      call sum512346(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x40,u145,1.000)
       deallocate(u145)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z350(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x40,d2,z350)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + x40(m,d,b,l,k,i)*t2b(c,a,m,j)          !mdblkicamj      (+1.000)
     &     - x40(m,d,a,l,k,i)*t2b(c,b,m,j)          !mdalkicbmj      (-1.000)
     &     - x40(m,c,b,l,k,i)*t2b(d,a,m,j)          !mcblkidamj      (-1.000)
     &     + x40(m,c,a,l,k,i)*t2b(d,b,m,j)          !mcalkidbmj      (+1.000)
     &     + x40(m,c,b,l,k,j)*t2b(d,a,m,i)          !mcblkjdami      (+1.000)
     &     - x40(m,c,a,l,k,j)*t2b(d,b,m,i)          !mcalkjdbmi      (-1.000)
     &     - x40(m,d,b,l,k,j)*t2b(c,a,m,i)          !mdblkjcami      (-1.000)
     &     + x40(m,d,a,l,k,j)*t2b(c,b,m,i)          !mdalkjcbmi      (+1.000)
     &     - x40(m,d,b,k,l,i)*t2b(c,a,m,j)          !mdbklicamj      (-1.000)
     &     + x40(m,d,a,k,l,i)*t2b(c,b,m,j)          !mdaklicbmj      (+1.000)
     &     + x40(m,c,b,k,l,i)*t2b(d,a,m,j)          !mcbklidamj      (+1.000)
     &     - x40(m,c,a,k,l,i)*t2b(d,b,m,j)          !mcaklidbmj      (-1.000)
     &     - x40(m,c,b,k,l,j)*t2b(d,a,m,i)          !mcbkljdami      (-1.000)
     &     + x40(m,c,a,k,l,j)*t2b(d,b,m,i)          !mcakljdbmi      (+1.000)
     &     + x40(m,d,b,k,l,j)*t2b(c,a,m,i)          !mdbkljcami      (+1.000)
     &     - x40(m,d,a,k,l,j)*t2b(c,b,m,i)          !mdakljcbmi      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum24713568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350, 1.000)
!       call sum23714568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350,-1.000)
!       call sum14723568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350,-1.000)
!       call sum13724568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350, 1.000)
!       call sum14823567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350, 1.000)
!       call sum13824567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350,-1.000)
!       call sum24813567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350,-1.000)
!       call sum23814567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350, 1.000)
!       call sum24713658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350,-1.000)
!       call sum23714658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350, 1.000)
!       call sum14723658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350, 1.000)
!       call sum13724658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350,-1.000)
!       call sum14823657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350,-1.000)
!       call sum13824657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350, 1.000)
!       call sum24813657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350, 1.000)
!       call sum23814657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z350,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z350(c,a,j,d,b,l,k,i)       ! 24713568 (+1.000)
!     & -z350(c,b,j,d,a,l,k,i)       ! 23714568 (-1.000)
!     & -z350(d,a,j,c,b,l,k,i)       ! 14723568 (-1.000)
!     & +z350(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!     & +z350(d,a,i,c,b,l,k,j)       ! 14823567 (+1.000)
!     & -z350(d,b,i,c,a,l,k,j)       ! 13824567 (-1.000)
!     & -z350(c,a,i,d,b,l,k,j)       ! 24813567 (-1.000)
!     & +z350(c,b,i,d,a,l,k,j)       ! 23814567 (+1.000)
!     & -z350(c,a,j,d,b,k,l,i)       ! 24713658 (-1.000)
!     & +z350(c,b,j,d,a,k,l,i)       ! 23714658 (+1.000)
!     & +z350(d,a,j,c,b,k,l,i)       ! 14723658 (+1.000)
!     & -z350(d,b,j,c,a,k,l,i)       ! 13724658 (-1.000)
!     & -z350(d,a,i,c,b,k,l,j)       ! 14823657 (-1.000)
!     & +z350(d,b,i,c,a,k,l,j)       ! 13824657 (+1.000)
!     & +z350(c,a,i,d,b,k,l,j)       ! 24813657 (+1.000)
!     & -z350(c,b,i,d,a,k,l,j)       ! 23814657 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z350)
       deallocate(x40)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s115(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s115)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n0,n1,s115,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder361245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
       allocate(u88(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k2*k4*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u88)
       deallocate(d1)
       deallocate(f2)
c
       call sum234516(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x41,u88,1.000)
       deallocate(u88)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3214(n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n0,n1,n0,n1,s115,d1)
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
       call sum21(n0,n1,n0,n1,x10,q21,-1.000)
       deallocate(q21)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3214(n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n0,n1,n0,n1,s115,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s117(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s117)
       deallocate(d1)
       deallocate(b2)
c
       call sum3241(n0,n1,n1,n3,n1,n3,n0,n1,x15,s117,-1.000)
c
       call sumx3142(n0,n3,n0,n1,n1,n3,n1,n3,n0,n1,x15,intr, 1.000)
c
!       allocate(h2(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       call reorder73124568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t4c,h2)
!       allocate(z15(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3
!       i2=k1*k2*k2*k3*k4*k4
!       i3=k3*k1
!       call egemm(i1,i2,i3,x15,h2,z15)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     - x15(m,e,b,i)*t4c(d,c,e,a,l,k,m,j)      !mebidcealkmj    (-1.000)
     &     + x15(m,e,a,i)*t4c(d,c,e,b,l,k,m,j)      !meaidceblkmj    (+1.000)
     &     + x15(m,e,b,j)*t4c(d,c,e,a,l,k,m,i)      !mebjdcealkmi    (+1.000)
     &     - x15(m,e,a,j)*t4c(d,c,e,b,l,k,m,i)      !meajdceblkmi    (-1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12456738(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z15,-1.000)
!       call sum12356748(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z15, 1.000)
!       call sum12456837(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z15, 1.000)
!       call sum12356847(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z15,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z15(d,c,a,l,k,j,b,i)       ! 12456738 (-1.000)
!     & +z15(d,c,b,l,k,j,a,i)       ! 12356748 (+1.000)
!     & +z15(d,c,a,l,k,i,b,j)       ! 12456837 (+1.000)
!     & -z15(d,c,b,l,k,i,a,j)       ! 12356847 (-1.000)
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
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4213(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s117,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s193(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s193)
       deallocate(d1)
       deallocate(b2)
       deallocate(s117)
c
       call sum2134(n1,n3,n1,n3,n1,n3,n0,n1,x6,s193, 1.000)
       deallocate(s193)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2314(n0,n1,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n0,n1,s115,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s116(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s116)
       deallocate(d1)
       deallocate(b2)
       deallocate(s115)
c
       call sum3241(n0,n1,n0,n1,n0,n1,n0,n1,x14,s116, 1.000)
c
       call sumx2143(n0,n3,n0,n1,n0,n1,n0,n1,n0,n1,x14,intr, 1.000)
c
!       allocate(h2(n0+1:n1,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2))
!       call reorder78123456(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n1,n0,n1,n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,t4c,h2)
!       allocate(z14(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1
!       i2=k2*k2*k3*k3*k4*k4
!       i3=k1*k1
!       call egemm(i1,i2,i3,x14,h2,z14)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum
     &     + (x14(n,m,j,i)*t4c(d,c,b,a,l,k,n,m))/2.0d0 !nmjidcbalknm    (+0.500)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       v4c=v4c+0.500*z14
!       deallocate(z14)
       deallocate(x14)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s116,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u176(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u176)
       deallocate(d1)
       deallocate(d2)
c
      call sum234156(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x47,u176,2.000)
       deallocate(u176)
c
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z389(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k4
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x47,d2,z389)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum                                !order 3,1,2,4
     &     + (x47(n,d,a,l,j,i)*t2b(c,b,k,n)          !ndaljicbkn      (+0.500)
     &     - x47(n,c,a,l,j,i)*t2b(d,b,k,n)           !ncaljidbkn      (-0.500)
     &     - x47(n,d,a,k,j,i)*t2b(c,b,l,n)           !ndakjicbln      (-0.500)
     &     + x47(n,c,a,k,j,i)*t2b(d,b,l,n))/2.0d0    !ncakjidbln      (+0.500)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13624578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z389,-0.500)
!       call sum23514678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z389,-0.500)
!       call sum23614578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z389, 0.500)
!       call sum13524678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z389, 0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z389(c,b,k,d,a,l,j,i)      ! 23614578 (+0.500) order 3,1,2,4
!     & -z389(d,b,k,c,a,l,j,i)       ! 13624578 (-0.500)
!     & -z389(c,b,l,d,a,k,j,i)       ! 23514678 (-0.500)
!     & +z389(d,b,l,c,a,k,j,i))/2.0d0! 13524678 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z389)
       deallocate(x47)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s116,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s192(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s192)
       deallocate(d1)
       deallocate(b2)
       deallocate(s116)
c
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x5,s192,-1.000)
       deallocate(s192)
c
       call sumx2143(n0,n3,n0,n1,n1,n3,n0,n1,n0,n1,x5,intr, 1.000)
c
!       allocate(f2(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
!       call reorder612345(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,t3c,f2)
!       allocate(z5(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k2*k2*k3*k4*k4
!       i3=k1
!       call egemm(i1,i2,i3,x5,f2,z5)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x5(m,b,j,i)*t3c(d,c,a,l,k,m)           !mbjidcalkm      (+1.000)
     &     - x5(m,a,j,i)*t3c(d,c,b,l,k,m)           !majidcblkm      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12456378(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z5, 1.000)
!       call sum12356478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z5,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z5(d,c,a,l,k,b,j,i)       ! 12456378 (+1.000)
!     & -z5(d,c,b,l,k,a,j,i)       ! 12356478 (-1.000)
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
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s118(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s118)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3421(n1,n3,n1,n3,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n1,n3,s118,d1)
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
       x11=x11+q22
       deallocate(q22)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder4231(n1,n3,n1,n3,n1,n3,n0,n1,
     & n0,n1,n1,n3,n1,n3,n1,n3,s118,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s119(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s119)
       deallocate(d1)
       deallocate(b2)
       deallocate(s118)
c
       call sum3124(n1,n3,n1,n3,n1,n3,n1,n3,x16,s119, 1.000)
       deallocate(s119)
c
       call sumx4321(n0,n3,n1,n3,n1,n3,n1,n3,n1,n3,x16,intr, 1.000)
c
!       allocate(h2(n1+1:n3,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       call reorder34125678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,n0,n1,t4c,h2)
!       allocate(z16(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1,
!     & n1+1:n3,n1+1:n3))
!       i1=k3*k3
!       i2=k1*k1*k2*k2*k4*k4
!       i3=k3*k3
!       call egemm(i1,i2,i3,x16,h2,z16)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do f=n1+1,n3
             sum=sum
     &     + (x16(f,e,b,a)*t4c(d,c,f,e,l,k,j,i))/2.0d0      !febadcfelkji    (+0.500)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12567834(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z16, 0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z16(d,c,l,k,j,i,b,a)/2.0d0)! 12567834 (+0.500)
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
       allocate(d1(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s120(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s120)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n0,n1,s120,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder142356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
       allocate(u110(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k2*k3*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u110)
       deallocate(d1)
       deallocate(f2)
c
      call sum234516(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x52,u110,1.000)
       deallocate(u110)
c
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z124(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k4
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x52,d2,z124)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - x52(m,c,a,l,j,i)*t2b(d,b,k,m)          !mcaljidbkm      (-1.000)
     &     + x52(m,c,b,l,j,i)*t2b(d,a,k,m)          !mcbljidakm      (+1.000)
     &     + x52(m,d,a,l,j,i)*t2b(c,b,k,m)          !mdaljicbkm      (+1.000)
     &     - x52(m,d,b,l,j,i)*t2b(c,a,k,m)          !mdbljicakm      (-1.000)
     &     + x52(m,c,a,k,j,i)*t2b(d,b,l,m)          !mcakjidblm      (+1.000)
     &     - x52(m,c,b,k,j,i)*t2b(d,a,l,m)          !mcbkjidalm      (-1.000)
     &     - x52(m,d,a,k,j,i)*t2b(c,b,l,m)          !mdakjicblm      (-1.000)
     &     + x52(m,d,b,k,j,i)*t2b(c,a,l,m)          !mdbkjicalm      (+1.000)
     &     + x52(m,c,a,l,i,j)*t2b(d,b,k,m)          !mcalijdbkm      (+1.000)
     &     - x52(m,c,b,l,i,j)*t2b(d,a,k,m)          !mcblijdakm      (-1.000)
     &     - x52(m,d,a,l,i,j)*t2b(c,b,k,m)          !mdalijcbkm      (-1.000)
     &     + x52(m,d,b,l,i,j)*t2b(c,a,k,m)          !mdblijcakm      (+1.000)
     &     - x52(m,c,a,k,i,j)*t2b(d,b,l,m)          !mcakijdblm      (-1.000)
     &     + x52(m,c,b,k,i,j)*t2b(d,a,l,m)          !mcbkijdalm      (+1.000)
     &     + x52(m,d,a,k,i,j)*t2b(c,b,l,m)          !mdakijcblm      (+1.000)
     &     - x52(m,d,b,k,i,j)*t2b(c,a,l,m)          !mdbkijcalm      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13624578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124,-1.000)
!       call sum14623578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124, 1.000)
!       call sum23614578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124, 1.000)
!       call sum24613578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124,-1.000)
!       call sum13524678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124, 1.000)
!       call sum14523678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124,-1.000)
!       call sum23514678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124,-1.000)
!       call sum24513678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124, 1.000)
!       call sum13624587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124, 1.000)
!       call sum14623587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124,-1.000)
!       call sum23614587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124,-1.000)
!       call sum24613587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124, 1.000)
!       call sum13524687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124,-1.000)
!       call sum14523687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124, 1.000)
!       call sum23514687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124, 1.000)
!       call sum24513687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z124,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z124(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!     & +z124(d,a,k,c,b,l,j,i)       ! 14623578 (+1.000)
!     & +z124(c,b,k,d,a,l,j,i)       ! 23614578 (+1.000)
!     & -z124(c,a,k,d,b,l,j,i)       ! 24613578 (-1.000)
!     & +z124(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & -z124(d,a,l,c,b,k,j,i)       ! 14523678 (-1.000)
!     & -z124(c,b,l,d,a,k,j,i)       ! 23514678 (-1.000)
!     & +z124(c,a,l,d,b,k,j,i)       ! 24513678 (+1.000)
!     & +z124(d,b,k,c,a,l,i,j)       ! 13624587 (+1.000)
!     & -z124(d,a,k,c,b,l,i,j)       ! 14623587 (-1.000)
!     & -z124(c,b,k,d,a,l,i,j)       ! 23614587 (-1.000)
!     & +z124(c,a,k,d,b,l,i,j)       ! 24613587 (+1.000)
!     & -z124(d,b,l,c,a,k,i,j)       ! 13524687 (-1.000)
!     & +z124(d,a,l,c,b,k,i,j)       ! 14523687 (+1.000)
!     & +z124(c,b,l,d,a,k,i,j)       ! 23514687 (+1.000)
!     & -z124(c,a,l,d,b,k,i,j)       ! 24513687 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z124)
       deallocate(x52)
c
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder3214(n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n2,n3,n0,n1,n0,n2,s120,d1)
       allocate(f2(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder612345(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,t3c,f2)
       allocate(u109(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k2*k2*k3*k4
       i3=k4*k1
       call egemm(i1,i2,i3,d1,f2,u109)
       deallocate(d1)
       deallocate(f2)
c
      call sum234561(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x34,u109,1.000)
       deallocate(u109)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z287(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x34,d2,z287)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - x34(n,c,b,l,k,i)*t2b(d,a,n,j)          !ncblkidanj      (-1.000)
     &     + x34(n,c,a,l,k,i)*t2b(d,b,n,j)          !ncalkidbnj      (+1.000)
     &     + x34(n,d,b,l,k,i)*t2b(c,a,n,j)          !ndblkicanj      (+1.000)
     &     - x34(n,d,a,l,k,i)*t2b(c,b,n,j)          !ndalkicbnj      (-1.000)
     &     + x34(n,c,b,l,k,j)*t2b(d,a,n,i)          !ncblkjdani      (+1.000)
     &     - x34(n,c,a,l,k,j)*t2b(d,b,n,i)          !ncalkjdbni      (-1.000)
     &     - x34(n,d,b,l,k,j)*t2b(c,a,n,i)          !ndblkjcani      (-1.000)
     &     + x34(n,d,a,l,k,j)*t2b(c,b,n,i)          !ndalkjcbni      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14723568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z287,-1.000)
!       call sum13724568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z287, 1.000)
!       call sum24713568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z287, 1.000)
!       call sum23714568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z287,-1.000)
!       call sum14823567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z287, 1.000)
!       call sum13824567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z287,-1.000)
!       call sum24813567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z287,-1.000)
!       call sum23814567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z287, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z287(d,a,j,c,b,l,k,i)       ! 14723568 (-1.000)
!     & +z287(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!     & +z287(c,a,j,d,b,l,k,i)       ! 24713568 (+1.000)
!     & -z287(c,b,j,d,a,l,k,i)       ! 23714568 (-1.000)
!     & +z287(d,a,i,c,b,l,k,j)       ! 14823567 (+1.000)
!     & -z287(d,b,i,c,a,l,k,j)       ! 13824567 (-1.000)
!     & -z287(c,a,i,d,b,l,k,j)       ! 24813567 (-1.000)
!     & +z287(c,b,i,d,a,l,k,j)       ! 23814567 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z287)
       deallocate(x34)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n0,n1,s120,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder142356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
       allocate(u90(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k2*k4*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u90)
       deallocate(d1)
       deallocate(f2)
c
       call sum234516(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x41,u90,1.000)
       deallocate(u90)
c
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z85(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k4*k4
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x41,d2,z85)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x41(m,d,c,l,k,i)*t2a(b,a,m,j)          !mdclkibamj      (+1.000)
     &     - x41(m,d,c,l,k,j)*t2a(b,a,m,i)          !mdclkjbami      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34712568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z85, 1.000)
!       call sum34812567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z85,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z85(b,a,j,d,c,l,k,i)       ! 34712568 (+1.000)
!     & -z85(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z85)
       deallocate(x41)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n0,n1,s120,d1)
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
       x10=x10+q23
       deallocate(q23)
c
       call sumx12(0,n3,n0,n1,n0,n1,x10,fockr, 1.000)
c
!       allocate(h2(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       call reorder71234568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n1,n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,t4c,h2)
!       allocate(z10(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1
!       i2=k1*k2*k2*k3*k3*k4*k4
!       i3=k1
!       call egemm(i1,i2,i3,x10,h2,z10)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x10(m,i)*t4c(d,c,b,a,l,k,m,j)          !midcbalkmj      (+1.000)
     &     - x10(m,j)*t4c(d,c,b,a,l,k,m,i)          !mjdcbalkmi      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       v4c=v4c+z10
!       call sum12345687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z10,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z10(d,c,b,a,l,k,j,i)       ! 12345678 (+1.000)
!     & -z10(d,c,b,a,l,k,i,j)       ! 12345687 (-1.000)
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
       allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n2,n0,n1,n2,n3,n0,n1,s120,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s131(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s131)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n2,n3,n2,n3,n0,n1,x18,s131,-1.000)
       deallocate(s131)
c
       call sumx1342(n0,n3,n0,n1,n2,n3,n2,n3,n0,n1,x18,intm, 1.000)
c
!       allocate(h2(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       call reorder71234568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n1,n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,t4c,h2)
!       allocate(z18(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,
!     & n2+1:n3,n0+1:n1))
!       i1=k1*k4
!       i2=k1*k2*k2*k3*k3*k4
!       i3=k4*k1
!       call egemm(i1,i2,i3,x18,h2,z18)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n1
             sum=sum
     &     + x18(m,e,d,i)*t4c(e,c,b,a,l,k,m,j)      !mediecbalkmj    (+1.000)
     &     - x18(m,e,c,i)*t4c(e,d,b,a,l,k,m,j)      !meciedbalkmj    (-1.000)
     &     - x18(m,e,d,j)*t4c(e,c,b,a,l,k,m,i)      !medjecbalkmi    (-1.000)
     &     + x18(m,e,c,j)*t4c(e,d,b,a,l,k,m,i)      !mecjedbalkmi    (+1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23456718(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z18, 1.000)
!       call sum13456728(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z18,-1.000)
!       call sum23456817(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z18,-1.000)
!       call sum13456827(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z18, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z18(c,b,a,l,k,j,d,i)       ! 23456718 (+1.000)
!     & -z18(d,b,a,l,k,j,c,i)       ! 13456728 (-1.000)
!     & -z18(c,b,a,l,k,i,d,j)       ! 23456817 (-1.000)
!     & +z18(d,b,a,l,k,i,c,j)       ! 13456827 (+1.000)
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
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder2314(n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n1,n0,n1,n0,n2,s120,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s130(n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s130)
       deallocate(d1)
       deallocate(b2)
c
       call sum3241(n0,n2,n0,n1,n0,n2,n0,n1,x17,s130, 1.000)
c
       call sumx2143(n0,n3,n0,n2,n0,n1,n0,n2,n0,n1,x17,intm, 1.000)
c
!       allocate(h2(n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       call reorder57123468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n2,n0,n1,n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t4c,h2)
!       allocate(z17(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2
!       i2=k1*k2*k3*k3*k4*k4
!       i3=k1*k2
!       call egemm(i1,i2,i3,x17,h2,z17)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     + x17(n,m,k,i)*t4c(d,c,b,a,n,l,m,j)      !nmkidcbanlmj    (+1.000)
     &     - x17(n,m,l,i)*t4c(d,c,b,a,n,k,m,j)      !nmlidcbankmj    (-1.000)
     &     - x17(n,m,k,j)*t4c(d,c,b,a,n,l,m,i)      !nmkjdcbanlmi    (-1.000)
     &     + x17(n,m,l,j)*t4c(d,c,b,a,n,k,m,i)      !nmljdcbankmi    (+1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12345768(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z17, 1.000)
!       call sum12346758(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z17,-1.000)
!       call sum12345867(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z17,-1.000)
!       call sum12346857(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z17, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z17(d,c,b,a,l,j,k,i)       ! 12345768 (+1.000)
!     & -z17(d,c,b,a,k,j,l,i)       ! 12346758 (-1.000)
!     & -z17(d,c,b,a,l,i,k,j)       ! 12345867 (-1.000)
!     & +z17(d,c,b,a,k,i,l,j)       ! 12346857 (+1.000)
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
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder2413(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s130,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u185(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u185)
       deallocate(d1)
       deallocate(d2)
c
      call sum234156(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x28,u185,1.000)
       deallocate(u185)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z282(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x28,d2,z282)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - x28(n,c,a,l,k,i)*t2b(d,b,n,j)          !ncalkidbnj      (-1.000)
     &     + x28(n,c,b,l,k,i)*t2b(d,a,n,j)          !ncblkidanj      (+1.000)
     &     + x28(n,d,a,l,k,i)*t2b(c,b,n,j)          !ndalkicbnj      (+1.000)
     &     - x28(n,d,b,l,k,i)*t2b(c,a,n,j)          !ndblkicanj      (-1.000)
     &     + x28(n,c,a,k,l,i)*t2b(d,b,n,j)          !ncaklidbnj      (+1.000)
     &     - x28(n,c,b,k,l,i)*t2b(d,a,n,j)          !ncbklidanj      (-1.000)
     &     - x28(n,d,a,k,l,i)*t2b(c,b,n,j)          !ndaklicbnj      (-1.000)
     &     + x28(n,d,b,k,l,i)*t2b(c,a,n,j)          !ndbklicanj      (+1.000)
     &     + x28(n,c,a,l,k,j)*t2b(d,b,n,i)          !ncalkjdbni      (+1.000)
     &     - x28(n,c,b,l,k,j)*t2b(d,a,n,i)          !ncblkjdani      (-1.000)
     &     - x28(n,d,a,l,k,j)*t2b(c,b,n,i)          !ndalkjcbni      (-1.000)
     &     + x28(n,d,b,l,k,j)*t2b(c,a,n,i)          !ndblkjcani      (+1.000)
     &     - x28(n,c,a,k,l,j)*t2b(d,b,n,i)          !ncakljdbni      (-1.000)
     &     + x28(n,c,b,k,l,j)*t2b(d,a,n,i)          !ncbkljdani      (+1.000)
     &     + x28(n,d,a,k,l,j)*t2b(c,b,n,i)          !ndakljcbni      (+1.000)
     &     - x28(n,d,b,k,l,j)*t2b(c,a,n,i)          !ndbkljcani      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13724568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282,-1.000)
!       call sum14723568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282, 1.000)
!       call sum23714568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282, 1.000)
!       call sum24713568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282,-1.000)
!       call sum13724658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282, 1.000)
!       call sum14723658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282,-1.000)
!       call sum23714658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282,-1.000)
!       call sum24713658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282, 1.000)
!       call sum13824567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282, 1.000)
!       call sum14823567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282,-1.000)
!       call sum23814567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282,-1.000)
!       call sum24813567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282, 1.000)
!       call sum13824657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282,-1.000)
!       call sum14823657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282, 1.000)
!       call sum23814657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282, 1.000)
!       call sum24813657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z282,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z282(d,b,j,c,a,l,k,i)       ! 13724568 (-1.000)
!     & +z282(d,a,j,c,b,l,k,i)       ! 14723568 (+1.000)
!     & +z282(c,b,j,d,a,l,k,i)       ! 23714568 (+1.000)
!     & -z282(c,a,j,d,b,l,k,i)       ! 24713568 (-1.000)
!     & +z282(d,b,j,c,a,k,l,i)       ! 13724658 (+1.000)
!     & -z282(d,a,j,c,b,k,l,i)       ! 14723658 (-1.000)
!     & -z282(c,b,j,d,a,k,l,i)       ! 23714658 (-1.000)
!     & +z282(c,a,j,d,b,k,l,i)       ! 24713658 (+1.000)
!     & +z282(d,b,i,c,a,l,k,j)       ! 13824567 (+1.000)
!     & -z282(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & -z282(c,b,i,d,a,l,k,j)       ! 23814567 (-1.000)
!     & +z282(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z282(d,b,i,c,a,k,l,j)       ! 13824657 (-1.000)
!     & +z282(d,a,i,c,b,k,l,j)       ! 14823657 (+1.000)
!     & +z282(c,b,i,d,a,k,l,j)       ! 23814657 (+1.000)
!     & -z282(c,a,i,d,b,k,l,j)       ! 24813657 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z282)
       deallocate(x28)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder2413(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s130,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u181(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u181)
       deallocate(d1)
       deallocate(d2)
c
      call sum235146(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x29,u181,1.000)
       deallocate(u181)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z264(n2+1:n3,n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k3
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x29,d2,z264)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     + x29(n,b,a,k,j,i)*t2c(d,c,n,l)          !nbakjidcnl      (+1.000)
     &     - x29(n,b,a,l,j,i)*t2c(d,c,n,k)          !nbaljidcnk      (-1.000)
     &     - x29(n,b,a,k,i,j)*t2c(d,c,n,l)          !nbakijdcnl      (-1.000)
     &     + x29(n,b,a,l,i,j)*t2c(d,c,n,k)          !nbalijdcnk      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12534678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z264, 1.000)
!       call sum12634578(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z264,-1.000)
!       call sum12534687(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z264,-1.000)
!       call sum12634587(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z264, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z264(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & -z264(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & -z264(d,c,l,b,a,k,i,j)       ! 12534687 (-1.000)
!     & +z264(d,c,k,b,a,l,i,j)       ! 12634587 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z264)
       deallocate(x29)
c
       allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       call reorder4213(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n2,n0,n1,n0,n2,n0,n1,s130,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s196(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s196)
       deallocate(d1)
       deallocate(b2)
       deallocate(s130)
c
       call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s196,-1.000)
       deallocate(s196)
c
       call sumx1243(n0,n3,n0,n1,n2,n3,n0,n2,n0,n1,x1,intm,1.000)
c
!       allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder512346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
!       allocate(z1(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k4
!       i2=k1*k2*k3*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x1,f2,z1)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - x1(m,d,k,i)*t3b(c,b,a,l,m,j)           !mdkicbalmj      (-1.000)
     &     + x1(m,c,k,i)*t3b(d,b,a,l,m,j)           !mckidbalmj      (+1.000)
     &     + x1(m,d,l,i)*t3b(c,b,a,k,m,j)           !mdlicbakmj      (+1.000)
     &     - x1(m,c,l,i)*t3b(d,b,a,k,m,j)           !mclidbakmj      (-1.000)
     &     + x1(m,d,k,j)*t3b(c,b,a,l,m,i)           !mdkjcbalmi      (+1.000)
     &     - x1(m,c,k,j)*t3b(d,b,a,l,m,i)           !mckjdbalmi      (-1.000)
     &     - x1(m,d,l,j)*t3b(c,b,a,k,m,i)           !mdljcbakmi      (-1.000)
     &     + x1(m,c,l,j)*t3b(d,b,a,k,m,i)           !mcljdbakmi      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23457168(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z1,-1.000)
!       call sum13457268(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z1, 1.000)
!       call sum23467158(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z1, 1.000)
!       call sum13467258(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z1,-1.000)
!       call sum23458167(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z1, 1.000)
!       call sum13458267(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z1,-1.000)
!       call sum23468157(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z1,-1.000)
!       call sum13468257(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z1, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z1(c,b,a,l,j,d,k,i)       ! 23457168 (-1.000)
!     & +z1(d,b,a,l,j,c,k,i)       ! 13457268 (+1.000)
!     & +z1(c,b,a,k,j,d,l,i)       ! 23467158 (+1.000)
!     & -z1(d,b,a,k,j,c,l,i)       ! 13467258 (-1.000)
!     & +z1(c,b,a,l,i,d,k,j)       ! 23458167 (+1.000)
!     & -z1(d,b,a,l,i,c,k,j)       ! 13458267 (-1.000)
!     & -z1(c,b,a,k,i,d,l,j)       ! 23468157 (-1.000)
!     & +z1(d,b,a,k,i,c,l,j)       ! 13468257 (+1.000)
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
c
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder3214(n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n2,n3,n0,n1,n0,n2,s120,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s121(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s121)
       deallocate(d1)
       deallocate(b2)
       deallocate(s120)
c
       call sum3241(n0,n2,n2,n3,n1,n3,n0,n1,x24,s121,-1.000)
c
       call sumx3142(n0,n3,n0,n2,n2,n3,n1,n3,n0,n1,x24,intm, 1.000)
c
!       allocate(h2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       call reorder51234678(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n2,n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t4d,h2)
!       allocate(z24(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3
!       i2=k1*k2*k2*k3*k4*k4
!       i3=k4*k2
!       call egemm(i1,i2,i3,x24,h2,z24)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     - x24(m,e,b,i)*t4d(e,d,c,a,m,l,k,j)      !mebiedcamlkj    (-1.000)
     &     + x24(m,e,a,i)*t4d(e,d,c,b,m,l,k,j)      !meaiedcbmlkj    (+1.000)
     &     + x24(m,e,b,j)*t4d(e,d,c,a,m,l,k,i)      !mebjedcamlki    (+1.000)
     &     - x24(m,e,a,j)*t4d(e,d,c,b,m,l,k,i)      !meajedcbmlki    (-1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12456738(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z24,-1.000)
!       call sum12356748(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z24, 1.000)
!       call sum12456837(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z24, 1.000)
!       call sum12356847(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z24,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z24(d,c,a,l,k,j,b,i)       ! 12456738 (-1.000)
!     & +z24(d,c,b,l,k,j,a,i)       ! 12356748 (+1.000)
!     & +z24(d,c,a,l,k,i,b,j)       ! 12456837 (+1.000)
!     & -z24(d,c,b,l,k,i,a,j)       ! 12356847 (-1.000)
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
       deallocate(x24)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder4213(n1,n3,n2,n3,n0,n1,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n1,s121,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s195(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s195)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x8,s195, 1.000)
       deallocate(s195)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2413(n1,n3,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s121,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s194(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s194)
       deallocate(d1)
       deallocate(b2)
       deallocate(s121)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x7,s194,-1.000)
       deallocate(s194)
c
       call sumx2143(n0,n3,n0,n2,n1,n3,n0,n2,n0,n1,x7,intm, 1.000)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
!       allocate(z7(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3
!       i2=k1*k2*k3*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x7,f2,z7)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     - x7(m,b,k,i)*t3c(d,c,a,m,l,j)           !mbkidcamlj      (-1.000)
     &     + x7(m,a,k,i)*t3c(d,c,b,m,l,j)           !makidcbmlj      (+1.000)
     &     + x7(m,b,l,i)*t3c(d,c,a,m,k,j)           !mblidcamkj      (+1.000)
     &     - x7(m,a,l,i)*t3c(d,c,b,m,k,j)           !malidcbmkj      (-1.000)
     &     + x7(m,b,k,j)*t3c(d,c,a,m,l,i)           !mbkjdcamli      (+1.000)
     &     - x7(m,a,k,j)*t3c(d,c,b,m,l,i)           !makjdcbmli      (-1.000)
     &     - x7(m,b,l,j)*t3c(d,c,a,m,k,i)           !mbljdcamki      (-1.000)
     &     + x7(m,a,l,j)*t3c(d,c,b,m,k,i)           !maljdcbmki      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12457368(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z7,-1.000)
!       call sum12357468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z7, 1.000)
!       call sum12467358(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z7, 1.000)
!       call sum12367458(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z7,-1.000)
!       call sum12458367(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z7, 1.000)
!       call sum12358467(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z7,-1.000)
!       call sum12468357(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z7,-1.000)
!       call sum12368457(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z7, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z7(d,c,a,l,j,b,k,i)       ! 12457368 (-1.000)
!     & +z7(d,c,b,l,j,a,k,i)       ! 12357468 (+1.000)
!     & +z7(d,c,a,k,j,b,l,i)       ! 12467358 (+1.000)
!     & -z7(d,c,b,k,j,a,l,i)       ! 12367458 (-1.000)
!     & +z7(d,c,a,l,i,b,k,j)       ! 12458367 (+1.000)
!     & -z7(d,c,b,l,i,a,k,j)       ! 12358467 (-1.000)
!     & -z7(d,c,a,k,i,b,l,j)       ! 12468357 (-1.000)
!     & +z7(d,c,b,k,i,a,l,j)       ! 12368457 (+1.000)
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
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s132(n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s132)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder2431(n0,n2,n1,n3,n0,n1,n0,n2,
     & n1,n3,n0,n2,n0,n1,n0,n2,s132,d1)
       allocate(f2(n1+1:n3,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder341256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n1,n3,n0,n2,n2,n3,n2,n3,n0,n2,n0,n1,t3c,f2)
       allocate(u128(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k2*k4*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,f2,u128)
       deallocate(d1)
       deallocate(f2)
c
      call sum234615(n0,n1,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,x45,u128,1.000)
       deallocate(u128)
c
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z93(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k4*k4
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x45,d2,z93)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - x45(m,d,c,l,k,j)*t2a(b,a,m,i)          !mdclkjbami      (-1.000)
     &     + x45(m,d,c,k,l,j)*t2a(b,a,m,i)          !mdckljbami      (+1.000)
     &     + x45(m,d,c,l,k,i)*t2a(b,a,m,j)          !mdclkibamj      (+1.000)
     &     - x45(m,d,c,k,l,i)*t2a(b,a,m,j)          !mdcklibamj      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34812567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z93,-1.000)
!       call sum34812657(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z93, 1.000)
!       call sum34712568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z93, 1.000)
!       call sum34712658(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z93,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z93(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & +z93(b,a,i,d,c,k,l,j)       ! 34812657 (+1.000)
!     & +z93(b,a,j,d,c,l,k,i)       ! 34712568 (+1.000)
!     & -z93(b,a,j,d,c,k,l,i)       ! 34712658 (-1.000)
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
       deallocate(x45)
c
       allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder4321(n0,n2,n1,n3,n0,n1,n0,n2,
     & n0,n2,n0,n1,n1,n3,n0,n2,s132,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s164(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s164)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n1,n3,n2,n3,n0,n2,x9,s164,-1.000)
       deallocate(s164)
c
       call sumx1324(n0,n3,n0,n1,n1,n3,n2,n3,n0,n2,x9,intm, 1.000)
c
!       allocate(h2(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       call reorder62134578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4b,h2)
!       allocate(z9(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,
!     & n2+1:n3,n0+1:n2))
!       i1=k2*k4
!       i2=k1*k1*k2*k3*k3*k4
!       i3=k3*k1
!       call egemm(i1,i2,i3,x9,h2,z9)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     - x9(m,e,d,k)*t4b(c,e,b,a,l,m,j,i)       !medkcebalmji    (-1.000)
     &     + x9(m,e,c,k)*t4b(d,e,b,a,l,m,j,i)       !meckdebalmji    (+1.000)
     &     + x9(m,e,d,l)*t4b(c,e,b,a,k,m,j,i)       !medlcebakmji    (+1.000)
     &     - x9(m,e,c,l)*t4b(d,e,b,a,k,m,j,i)       !mecldebakmji    (-1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23457816(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z9,-1.000)
!       call sum13457826(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z9, 1.000)
!       call sum23467815(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z9, 1.000)
!       call sum13467825(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z9,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z9(c,b,a,l,j,i,d,k)       ! 23457816 (-1.000)
!     & +z9(d,b,a,l,j,i,c,k)       ! 13457826 (+1.000)
!     & +z9(c,b,a,k,j,i,d,l)       ! 23467815 (+1.000)
!     & -z9(d,b,a,k,j,i,c,l)       ! 13467825 (-1.000)
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
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder3214(n0,n2,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n0,n2,n0,n2,s132,d1)
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
       call sum21(n0,n2,n0,n2,x12,q24, 1.000)
       deallocate(q24)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder3214(n0,n2,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n0,n2,n0,n2,s132,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s133(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s133)
       deallocate(d1)
       deallocate(b2)
       deallocate(s132)
c
       call sum3241(n0,n2,n1,n3,n1,n3,n0,n2,x19,s133,-1.000)
c
       call sumx3124(n0,n3,n0,n2,n1,n3,n1,n3,n0,n2,x19,intm, 1.000)
c
!       allocate(h2(n0+1:n2,n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       call reorder53124678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n2,n1,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4c,h2)
!       allocate(z19(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,
!     & n1+1:n3,n0+1:n2))
!       i1=k2*k3
!       i2=k1*k1*k2*k3*k4*k4
!       i3=k3*k2
!       call egemm(i1,i2,i3,x19,h2,z19)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n2
             sum=sum
     &     + x19(m,e,b,k)*t4c(d,c,e,a,m,l,j,i)      !mebkdceamlji    (+1.000)
     &     - x19(m,e,a,k)*t4c(d,c,e,b,m,l,j,i)      !meakdcebmlji    (-1.000)
     &     - x19(m,e,b,l)*t4c(d,c,e,a,m,k,j,i)      !mebldceamkji    (-1.000)
     &     + x19(m,e,a,l)*t4c(d,c,e,b,m,k,j,i)      !mealdcebmkji    (+1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12457836(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z19, 1.000)
!       call sum12357846(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z19,-1.000)
!       call sum12467835(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z19,-1.000)
!       call sum12367845(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z19, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z19(d,c,a,l,j,i,b,k)       ! 12457836 (+1.000)
!     & -z19(d,c,b,l,j,i,a,k)       ! 12357846 (-1.000)
!     & -z19(d,c,a,k,j,i,b,l)       ! 12467835 (-1.000)
!     & +z19(d,c,b,k,j,i,a,l)       ! 12367845 (+1.000)
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
       allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4213(n1,n3,n1,n3,n0,n2,n0,n2,
     & n0,n2,n1,n3,n1,n3,n0,n2,s133,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s197(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s197)
       deallocate(d1)
       deallocate(b2)
       deallocate(s133)
c
       call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s197, 1.000)
       deallocate(s197)
c
       allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s134(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s134)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       call reorder2431(n1,n3,n2,n3,n1,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n1,n3,s134,d1)
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
       x11=x11-q27
       deallocate(q27)
c
       call sumx21(0,n3,n1,n3,n1,n3,x11,fockr, 1.000)
c
!       allocate(h2(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       call reorder31245678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n1,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,t4c,h2)
!       allocate(z11(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,
!     & n0+1:n1,n1+1:n3))
!       i1=k3
!       i2=k1*k1*k2*k2*k3*k4*k4
!       i3=k3
!       call egemm(i1,i2,i3,x11,h2,z11)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + x11(e,b)*t4c(d,c,e,a,l,k,j,i)          !ebdcealkji      (+1.000)
     &     - x11(e,a)*t4c(d,c,e,b,l,k,j,i)          !eadceblkji      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12456783(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z11, 1.000)
!       call sum12356784(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z11,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z11(d,c,a,l,k,j,i,b)       ! 12456783 (+1.000)
!     & -z11(d,c,b,l,k,j,i,a)       ! 12356784 (-1.000)
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
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
       call reorder4231(n1,n3,n2,n3,n1,n3,n0,n2,
     & n0,n2,n2,n3,n1,n3,n1,n3,s134,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s135(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s135)
       deallocate(d1)
       deallocate(b2)
       deallocate(s134)
c
       call sum3124(n2,n3,n1,n3,n2,n3,n1,n3,x20,s135, 1.000)
       deallocate(s135)
c
       call sumx4321(n0,n3,n2,n3,n1,n3,n2,n3,n1,n3,x20,intm, 1.000)
c
!       allocate(h2(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       call reorder13245678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,t4c,h2)
!       allocate(z20(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1,
!     & n2+1:n3,n1+1:n3))
!       i1=k3*k4
!       i2=k1*k1*k2*k2*k3*k4
!       i3=k3*k4
!       call egemm(i1,i2,i3,x20,h2,z20)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do f=n2+1,n3
             sum=sum
     &     + x20(f,e,d,b)*t4c(f,c,e,a,l,k,j,i)      !fedbfcealkji    (+1.000)
     &     - x20(f,e,d,a)*t4c(f,c,e,b,l,k,j,i)      !fedafceblkji    (-1.000)
     &     - x20(f,e,c,b)*t4c(f,d,e,a,l,k,j,i)      !fecbfdealkji    (-1.000)
     &     + x20(f,e,c,a)*t4c(f,d,e,b,l,k,j,i)      !fecafdeblkji    (+1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum24567813(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z20, 1.000)
!       call sum23567814(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z20,-1.000)
!       call sum14567823(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z20,-1.000)
!       call sum13567824(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z20, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z20(c,a,l,k,j,i,d,b)       ! 24567813 (+1.000)
!     & -z20(c,b,l,k,j,i,d,a)       ! 23567814 (-1.000)
!     & -z20(d,a,l,k,j,i,c,b)       ! 14567823 (-1.000)
!     & +z20(d,b,l,k,j,i,c,a)       ! 13567824 (+1.000)
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
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s156(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
       i1=k4
       i2=k2*k4*k4
       i3=k2
       call egemm(i1,i2,i3,b1,d2,s156)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n2,n3,n2,n3,n2,n3,n0,n2,x4,s156,-1.000)
       deallocate(s156)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,q25,b1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s152(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,b1,d2,s152)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n2,n3,n2,n3,n1,n3,n0,n1,x8,s152,-1.000)
       deallocate(s152)
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
       call sum21(n2,n3,n2,n3,x13,q26,-1.000)
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
       allocate(s145(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
       i1=k3
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s145)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x2,s145,-1.000)
       deallocate(s145)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,q28,b1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s141(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s141)
       deallocate(b1)
       deallocate(d2)
       deallocate(q28)
c
       call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x6,s141,-1.000)
       deallocate(s141)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s165(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s165)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder3214(n0,n2,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n0,n2,n0,n2,s165,d1)
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
       call sum21(n0,n2,n0,n2,x12,q29,-1.000)
       deallocate(q29)
c
       call sumx12(0,n3,n0,n2,n0,n2,x12,fockb, 1.000)
c
!       allocate(h2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       call reorder51234678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n2,n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4c,h2)
!       allocate(z12(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,
!     & n0+1:n1,n0+1:n2))
!       i1=k2
!       i2=k1*k1*k2*k3*k3*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x12,h2,z12)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + x12(m,k)*t4c(d,c,b,a,m,l,j,i)          !mkdcbamlji      (+1.000)
     &     - x12(m,l)*t4c(d,c,b,a,m,k,j,i)          !mldcbamkji      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12345786(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z12, 1.000)
!       call sum12346785(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z12,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z12(d,c,b,a,l,j,i,k)       ! 12345786 (+1.000)
!     & -z12(d,c,b,a,k,j,i,l)       ! 12346785 (-1.000)
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
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder3214(n0,n2,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n0,n2,n0,n2,s165,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s167(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s167)
       deallocate(d1)
       deallocate(b2)
c
       call sum3241(n0,n2,n2,n3,n2,n3,n0,n2,x22,s167,-1.000)
c
       call sumx3142(n0,n3,n0,n2,n2,n3,n2,n3,n0,n2,x22,intb, 1.000)
c
!       allocate(h2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       call reorder51234678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n2,n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4c,h2)
!       allocate(z22(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,
!     & n2+1:n3,n0+1:n2))
!       i1=k2*k4
!       i2=k1*k1*k2*k3*k3*k4
!       i3=k4*k2
!       call egemm(i1,i2,i3,x22,h2,z22)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     - x22(m,e,d,k)*t4c(e,c,b,a,m,l,j,i)      !medkecbamlji    (-1.000)
     &     + x22(m,e,c,k)*t4c(e,d,b,a,m,l,j,i)      !meckedbamlji    (+1.000)
     &     + x22(m,e,d,l)*t4c(e,c,b,a,m,k,j,i)      !medlecbamkji    (+1.000)
     &     - x22(m,e,c,l)*t4c(e,d,b,a,m,k,j,i)      !mecledbamkji    (-1.000)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23457816(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z22,-1.000)
!       call sum13457826(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z22, 1.000)
!       call sum23467815(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z22, 1.000)
!       call sum13467825(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z22,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z22(c,b,a,l,j,i,d,k)       ! 23457816 (-1.000)
!     & +z22(d,b,a,l,j,i,c,k)       ! 13457826 (+1.000)
!     & +z22(c,b,a,k,j,i,d,l)       ! 23467815 (+1.000)
!     & -z22(d,b,a,k,j,i,c,l)       ! 13467825 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z22)
       deallocate(x22)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,s167,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s199(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s199)
       deallocate(d1)
       deallocate(b2)
       deallocate(s167)
c
       call sum2134(n2,n3,n2,n3,n2,n3,n0,n2,x4,s199, 1.000)
       deallocate(s199)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder2314(n0,n2,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n0,n2,n0,n2,s165,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s166(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s166)
       deallocate(d1)
       deallocate(b2)
       deallocate(s165)
c
       call sum3241(n0,n2,n0,n2,n0,n2,n0,n2,x21,s166, 1.000)
c
       call sumx2143(n0,n3,n0,n2,n0,n2,n0,n2,n0,n2,x21,intb, 1.000)
c
!       allocate(h2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       call reorder56123478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n2,n0,n2,n2,n3,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4c,h2)
!       allocate(z21(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2
!       i2=k1*k1*k3*k3*k4*k4
!       i3=k2*k2
!       call egemm(i1,i2,i3,x21,h2,z21)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2;do n=n0+1,n2
             sum=sum
     &     + (x21(n,m,l,k)*t4c(d,c,b,a,n,m,j,i))/2.0d0  !nmlkdcbanmji    (+0.500)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12347856(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z21, 0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z21(d,c,b,a,j,i,l,k)/2.0d0)! 12347856 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z21)
       deallocate(x21)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder2413(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s166,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(u191(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,d1,d2,u191)
       deallocate(d1)
       deallocate(d2)
c
      call sum236145(n0,n2,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,x49,u191,1.000)
       deallocate(u191)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z116(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k2*k3*k4
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x49,d2,z116)
!       deallocate(d2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - x49(n,c,a,l,k,j)*t2b(d,b,n,i)          !ncalkjdbni      (-1.000)
     &     - x49(n,d,a,l,k,i)*t2b(c,b,n,j)          !ndalkicbnj      (-1.000)
     &     + x49(n,d,a,l,k,j)*t2b(c,b,n,i)          !ndalkjcbni      (+1.000)
     &     + x49(n,c,a,l,k,i)*t2b(d,b,n,j)          !ncalkidbnj      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13824567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z116,-1.000)
!       call sum23714568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z116,-1.000)
!       call sum23814567(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z116, 1.000)
!       call sum13724568(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z116, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z116(d,b,i,c,a,l,k,j)       ! 13824567 (-1.000)
!     & -z116(c,b,j,d,a,l,k,i)       ! 23714568 (-1.000)
!     & +z116(c,b,i,d,a,l,k,j)       ! 23814567 (+1.000)
!     & +z116(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z116)
       deallocate(x49)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder2413(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s166,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s198(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s198)
       deallocate(d1)
       deallocate(b2)
       deallocate(s166)
c
       call sum2134(n0,n2,n2,n3,n0,n2,n0,n2,x3,s198,-1.000)
       deallocate(s198)
c
       call sumx2143(n0,n3,n0,n2,n2,n3,n0,n2,n0,n2,x3,intb, 1.000)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z3(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k1*k1*k3*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x3,f2,z3)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + x3(m,d,l,k)*t3b(c,b,a,m,j,i)           !mdlkcbamji      (+1.000)
     &     - x3(m,c,l,k)*t3b(d,b,a,m,j,i)           !mclkdbamji      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23478156(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z3, 1.000)
!       call sum13478256(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z3,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z3(c,b,a,j,i,d,l,k)       ! 23478156 (+1.000)
!     & -z3(d,b,a,j,i,c,l,k)       ! 13478256 (-1.000)
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
       allocate(s168(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s168)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3421(n2,n3,n2,n3,n2,n3,n0,n2,
     & n2,n3,n0,n2,n2,n3,n2,n3,s168,d1)
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
       x13=x13+q30
       deallocate(q30)
c
       call sumx21(0,n3,n2,n3,n2,n3,x13,fockb, 1.000)
c
!       allocate(h2(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       call reorder12345678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,t4c,h2)
!       allocate(z13(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,
!     & n0+1:n1,n2+1:n3))
!       i1=k4
!       i2=k1*k1*k2*k2*k3*k3*k4
!       i3=k4
!       call egemm(i1,i2,i3,x13,h2,z13)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     + x13(e,d)*t4c(e,c,b,a,l,k,j,i)          !edecbalkji      (+1.000)
     &     - x13(e,c)*t4c(e,d,b,a,l,k,j,i)          !ecedbalkji      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23456781(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z13, 1.000)
!       call sum13456782(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z13,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z13(c,b,a,l,k,j,i,d)       ! 23456781 (+1.000)
!     & -z13(d,b,a,l,k,j,i,c)       ! 13456782 (-1.000)
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
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder4231(n2,n3,n2,n3,n2,n3,n0,n2,
     & n0,n2,n2,n3,n2,n3,n2,n3,s168,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s169(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s169)
       deallocate(d1)
       deallocate(b2)
       deallocate(s168)
c
       call sum3124(n2,n3,n2,n3,n2,n3,n2,n3,x23,s169, 1.000)
       deallocate(s169)
c
       call sumx4321(n0,n3,n2,n3,n2,n3,n2,n3,n2,n3,x23,intb, 1.000)
c
!       allocate(h2(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       call reorder12345678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,t4c,h2)
!       allocate(z23(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1,
!     & n2+1:n3,n2+1:n3))
!       i1=k4*k4
!       i2=k1*k1*k2*k2*k3*k3
!       i3=k4*k4
!       call egemm(i1,i2,i3,x23,h2,z23)
!       deallocate(h2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do f=n2+1,n3
             sum=sum
     &     + (x23(f,e,d,c)*t4c(f,e,b,a,l,k,j,i))/2.0d0 !fedcfebalkji    (+0.500)
             enddo;enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34567812(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z23, 0.500)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +(z23(b,a,l,k,j,i,d,c)/2.0d0)! 34567812 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z23)
       deallocate(x23)
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
       allocate(s181(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
       i1=k3
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s181)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x2,s181,-1.000)
       deallocate(s181)
c
       call sumx3214(n0,n3,n1,n3,n2,n3,n1,n3,n0,n2,x2,intm, 1.000)
c
!       allocate(f2(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder213456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
!       allocate(z2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n1+1:n3,n0+1:n2))
!       i1=k2*k3*k4
!       i2=k1*k1*k2*k3*k4
!       i3=k3
!       call egemm(i1,i2,i3,x2,f2,z2)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     - x2(e,d,b,k)*t3b(c,e,a,l,j,i)           !edbkcealji      (-1.000)
     &     + x2(e,d,a,k)*t3b(c,e,b,l,j,i)           !edakceblji      (+1.000)
     &     + x2(e,c,b,k)*t3b(d,e,a,l,j,i)           !ecbkdealji      (+1.000)
     &     - x2(e,c,a,k)*t3b(d,e,b,l,j,i)           !ecakdeblji      (-1.000)
     &     + x2(e,d,b,l)*t3b(c,e,a,k,j,i)           !edblceakji      (+1.000)
     &     - x2(e,d,a,l)*t3b(c,e,b,k,j,i)           !edalcebkji      (-1.000)
     &     - x2(e,c,b,l)*t3b(d,e,a,k,j,i)           !ecbldeakji      (-1.000)
     &     + x2(e,c,a,l)*t3b(d,e,b,k,j,i)           !ecaldebkji      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum24578136(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z2,-1.000)
!       call sum23578146(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z2, 1.000)
!       call sum14578236(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z2, 1.000)
!       call sum13578246(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z2,-1.000)
!       call sum24678135(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z2, 1.000)
!       call sum23678145(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z2,-1.000)
!       call sum14678235(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z2,-1.000)
!       call sum13678245(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z2, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z2(c,a,l,j,i,d,b,k)       ! 24578136 (-1.000)
!     & +z2(c,b,l,j,i,d,a,k)       ! 23578146 (+1.000)
!     & +z2(d,a,l,j,i,c,b,k)       ! 14578236 (+1.000)
!     & -z2(d,b,l,j,i,c,a,k)       ! 13578246 (-1.000)
!     & +z2(c,a,k,j,i,d,b,l)       ! 24678135 (+1.000)
!     & -z2(c,b,k,j,i,d,a,l)       ! 23678145 (-1.000)
!     & -z2(d,a,k,j,i,c,b,l)       ! 14678235 (-1.000)
!     & +z2(d,b,k,j,i,c,a,l)       ! 13678245 (+1.000)
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
     & n0,n1,n1,n3,q31,b1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s173(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s173)
       deallocate(b1)
       deallocate(d2)
       deallocate(q31)
c
       call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x6,s173,-1.000)
       deallocate(s173)
c
       call sumx3241(n0,n3,n1,n3,n1,n3,n1,n3,n0,n1,x6,intr, 1.000)
c
!       allocate(f2(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder312456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
!       allocate(z6(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n1+1:n3,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k2*k2*k4*k4
!       i3=k3
!       call egemm(i1,i2,i3,x6,f2,z6)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + x6(e,b,a,i)*t3c(d,c,e,l,k,j)           !ebaidcelkj      (+1.000)
     &     - x6(e,b,a,j)*t3c(d,c,e,l,k,i)           !ebajdcelki      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12567348(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z6, 1.000)
!       call sum12568347(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z6,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z6(d,c,l,k,j,b,a,i)       ! 12567348 (+1.000)
!     & -z6(d,c,l,k,i,b,a,j)       ! 12568347 (-1.000)
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
       allocate(s191(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
       i1=k4
       i2=k2*k4*k4
       i3=k2
       call egemm(i1,i2,i3,b1,d2,s191)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n2,n3,n2,n3,n2,n3,n0,n2,x4,s191,-1.000)
       deallocate(s191)
c
       call sumx3241(n0,n3,n2,n3,n2,n3,n2,n3,n0,n2,x4,intb, 1.000)
c
!       allocate(f2(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder123456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
!       allocate(z4(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k1*k1*k2*k3*k3
!       i3=k4
!       call egemm(i1,i2,i3,x4,f2,z4)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     + x4(e,d,c,k)*t3b(e,b,a,l,j,i)           !edckebalji      (+1.000)
     &     - x4(e,d,c,l)*t3b(e,b,a,k,j,i)           !edclebakji      (-1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34578126(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z4, 1.000)
!       call sum34678125(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z4,-1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & +z4(b,a,l,j,i,d,c,k)       ! 34578126 (+1.000)
!     & -z4(b,a,k,j,i,d,c,l)       ! 34678125 (-1.000)
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
       allocate(s185(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,b1,d2,s185)
       deallocate(b1)
       deallocate(d2)
       deallocate(q32)
c
       call sum2341(n2,n3,n2,n3,n1,n3,n0,n1,x8,s185,-1.000)
       deallocate(s185)
c
       call sumx3241(n0,n3,n2,n3,n2,n3,n1,n3,n0,n1,x8,intm, 1.000)
c
!       allocate(f2(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
!       call reorder123456(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,t3c,f2)
!       allocate(z8(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n2+1:n3,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3*k4
!       i2=k1*k2*k2*k3*k4
!       i3=k4
!       call egemm(i1,i2,i3,x8,f2,z8)
!       deallocate(f2)
c
       do i=n0+1,n1-1;do j=i+1,n1;do k=n0+1,n2-1;do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-1;do b=a+1,n3;do c=n2+1,n3-1;do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     - x8(e,d,b,i)*t3c(e,c,a,l,k,j)           !edbiecalkj      (-1.000)
     &     + x8(e,d,a,i)*t3c(e,c,b,l,k,j)           !edaiecblkj      (+1.000)
     &     + x8(e,c,b,i)*t3c(e,d,a,l,k,j)           !ecbiedalkj      (+1.000)
     &     - x8(e,c,a,i)*t3c(e,d,b,l,k,j)           !ecaiedblkj      (-1.000)
     &     + x8(e,d,b,j)*t3c(e,c,a,l,k,i)           !edbjecalki      (+1.000)
     &     - x8(e,d,a,j)*t3c(e,c,b,l,k,i)           !edajecblki      (-1.000)
     &     - x8(e,c,b,j)*t3c(e,d,a,l,k,i)           !ecbjedalki      (-1.000)
     &     + x8(e,c,a,j)*t3c(e,d,b,l,k,i)           !ecajedblki      (+1.000)
             enddo
             v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum24567138(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z8,-1.000)
!       call sum23567148(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z8, 1.000)
!       call sum14567238(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z8, 1.000)
!       call sum13567248(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z8,-1.000)
!       call sum24568137(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z8, 1.000)
!       call sum23568147(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z8,-1.000)
!       call sum14568237(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z8,-1.000)
!       call sum13568247(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,n0,n1,v4c,z8, 1.000)
c
!       do i=n0+1,n1-1
!       do j=i+1,n1
!       do k=n0+1,n2-1
!       do l=k+1,n2
!       do a=n1+1,n3-1
!       do b=a+1,n3
!       do c=n2+1,n3-1
!       do d=c+1,n3
!c
!       v4c(d,c,b,a,l,k,j,i)=v4c(d,c,b,a,l,k,j,i)
!     & -z8(c,a,l,k,j,d,b,i)       ! 24567138 (-1.000)
!     & +z8(c,b,l,k,j,d,a,i)       ! 23567148 (+1.000)
!     & +z8(d,a,l,k,j,c,b,i)       ! 14567238 (+1.000)
!     & -z8(d,b,l,k,j,c,a,i)       ! 13567248 (-1.000)
!     & +z8(c,a,l,k,i,d,b,j)       ! 24568137 (+1.000)
!     & -z8(c,b,l,k,i,d,a,j)       ! 23568147 (-1.000)
!     & -z8(d,a,l,k,i,c,b,j)       ! 14568237 (-1.000)
!     & +z8(d,b,l,k,i,c,a,j)       ! 13568247 (+1.000)
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
c
       do i=n0+1,n1-1
       do j=i+1,n1
       do k=n0+1,n2-1
       do l=k+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
       do a=n1+1,n3-1
       do b=a+1,n3
       do c=n2+1,n3-1
       do d=c+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
!
!        iocca=0
!        ioccb=0
!        iunoa=0
!        iunob=0
!        if(i.gt.(n1-iactocca))iocca=iocca+1
!        if(j.gt.(n1-iactocca))iocca=iocca+1
!        if(k.gt.(n2-iactoccb))ioccb=ioccb+1
!        if(l.gt.(n2-iactoccb))ioccb=ioccb+1
!        if(iocca+ioccb.lt.iactindq)cycle
!        if(a.lt.(n1+iactunoa+1))iunoa=iunoa+1
!        if(b.lt.(n1+iactunoa+1))iunoa=iunoa+1
!        if(c.lt.(n2+iactunob+1))iunob=iunob+1
!        if(d.lt.(n2+iactunob+1))iunob=iunob+1
!        if(iunoa+iunob.lt.iactindq)cycle
!
         coeleft=fockb(d,d)
     &          +fockb(c,c)
     &          +fockr(b,b)
     &          +fockr(a,a)
     &          -fockb(l,l)
     &          -fockb(k,k)
     &          -fockr(j,j)
     &          -fockr(i,i)
     &          +shift
         t4c(d,c,b,a,l,k,j,i)=
     &   t4c(d,c,b,a,l,k,j,i)-v4c(d,c,b,a,l,k,j,i)/coeleft
         t4c(d,c,b,a,l,k,i,j)=-t4c(d,c,b,a,l,k,j,i)
         t4c(d,c,b,a,k,l,j,i)=-t4c(d,c,b,a,l,k,j,i)
         t4c(d,c,b,a,k,l,i,j)= t4c(d,c,b,a,l,k,j,i)
         t4c(d,c,a,b,l,k,j,i)=-t4c(d,c,b,a,l,k,j,i)
         t4c(d,c,a,b,l,k,i,j)= t4c(d,c,b,a,l,k,j,i)
         t4c(d,c,a,b,k,l,j,i)= t4c(d,c,b,a,l,k,j,i)
         t4c(d,c,a,b,k,l,i,j)=-t4c(d,c,b,a,l,k,j,i)
         t4c(c,d,b,a,l,k,j,i)=-t4c(d,c,b,a,l,k,j,i)
         t4c(c,d,b,a,l,k,i,j)= t4c(d,c,b,a,l,k,j,i)
         t4c(c,d,b,a,k,l,j,i)= t4c(d,c,b,a,l,k,j,i)
         t4c(c,d,b,a,k,l,i,j)=-t4c(d,c,b,a,l,k,j,i)
         t4c(c,d,a,b,l,k,j,i)= t4c(d,c,b,a,l,k,j,i)
         t4c(c,d,a,b,l,k,i,j)=-t4c(d,c,b,a,l,k,j,i)
         t4c(c,d,a,b,k,l,j,i)=-t4c(d,c,b,a,l,k,j,i)
         t4c(c,d,a,b,k,l,i,j)= t4c(d,c,b,a,l,k,j,i)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       rewind(tc)
       write(tc)t4c
c
       end
