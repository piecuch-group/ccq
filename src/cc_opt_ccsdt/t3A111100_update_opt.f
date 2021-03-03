       subroutine t3A111100_update(N0,N1,N2,N3,HT3A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t1A,t1B,t2A,t2B,t2C,
     & t3A,t3B1,t3B2,t3B3,t3B4,t3C1,t3C2,t3C3,t3C4,t3D)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 CoeLeft,shift,PP
       real*8 FAHH(N0+1:N1,N0+1:N1)
       real*8 FAHP(N1+1:N3,N0+1:N1)
       real*8 FAPP(N1+1:N3,N1+1:N3)
       real*8 FBHH(N0+1:N2,N0+1:N2)
       real*8 FBHP(N2+1:N3,N0+1:N2)
       real*8 FBPP(N2+1:N3,N2+1:N3)
       real*8 VAHHHH(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 VAHHHP(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 VAHHPP(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 VAHPHP(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1)
       real*8 VAHPPP(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1)
       real*8 VBHHHH(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1)
       real*8 VBHHHP(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1)
       real*8 VBHHPH(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 VBHHPP(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 VBHPHP(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1)
       real*8 VBHPPH(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1)
       real*8 VBPHPH(N0+1:N2,N1+1:N3,N0+1:N2,N1+1:N3)
       real*8 VBHPPP(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1)
       real*8 VBPHPP(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3)
       real*8 VCHHHH(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2)
       real*8 VCHHHP(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
       real*8 VCHHPP(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 VCHPHP(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2)
       real*8 VCHPPP(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2)
       real*8 VAAPPP(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:M2)
       real*8 VBAPPP(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:M2)
       real*8 VBPAPP(N2+1:N3,N1+1:N3,N2+1:M2,N1+1:N3)
       real*8 VCAPPP(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:M2)
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 t3A(N1+1:N3,N1+1:N3,N1+1:M2,N0+1:N1,N0+1:N1,M1+1:N1)
       real*8 t3B1(N2+1:N3,N1+1:N3,N1+1:M2,N0+1:N2,N0+1:N1,M1+1:N1)
       real*8 t3B2(N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1)
       real*8 t3B3(N2+1:M2,M2+1:N3,M2+1:N3,N0+1:N2,N0+1:N1,M1+1:N1)
       real*8 t3B4(N2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1)
       real*8 t3C1(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:N2,M1+1:N2,N0+1:N1)
       real*8 t3C2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1)
       real*8 t3C3(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:N2,M1+1:N2,N0+1:N1)
       real*8 t3C4(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N1)
       real*8 t3D(N2+1:N3,N2+1:N3,N2+1:M2,N0+1:N2,N0+1:N2,M1+1:N2)
       real*8 HT3A(N1+1:N3,N1+1:N3,N1+1:M2,N0+1:N1,N0+1:N1,M1+1:N1)
C
       real*8,allocatable::V3A(:,:,:,:,:,:)
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F1(:,:,:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
       real*8,allocatable::S1(:,:,:,:)
       real*8,allocatable::S2(:,:,:,:)
       real*8,allocatable::S3(:,:,:,:)
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::S5(:,:,:,:)
       real*8,allocatable::S6(:,:,:,:)
       real*8,allocatable::S7(:,:,:,:)
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::U59(:,:,:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S57(:,:,:,:)
       real*8,allocatable::S58(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::S60(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::S62(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::S64(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::S66(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::S68(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S70(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S72(:,:,:,:)
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::S74(:,:,:,:)
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S76(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::S78(:,:,:,:)
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::S80(:,:,:,:)
       real*8,allocatable::S81(:,:,:,:)
       real*8,allocatable::S82(:,:,:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::S84(:,:,:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::U133(:,:,:,:,:,:)
       real*8,allocatable::U211(:,:,:,:,:,:)
       real*8,allocatable::S87(:,:,:,:)
       real*8,allocatable::S88(:,:,:,:)
       real*8,allocatable::S89(:,:,:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::S90(:,:,:,:)
       real*8,allocatable::S91(:,:,:,:)
       real*8,allocatable::S92(:,:,:,:)
       real*8,allocatable::S93(:,:,:,:)
       real*8,allocatable::S94(:,:,:,:)
       real*8,allocatable::S95(:,:,:,:)
       real*8,allocatable::S96(:,:,:,:)
       real*8,allocatable::S97(:,:,:,:)
       real*8,allocatable::S98(:,:,:,:)
       real*8,allocatable::S99(:,:,:,:)
       real*8,allocatable::S100(:,:,:,:)
       real*8,allocatable::S101(:,:,:,:)
       real*8,allocatable::S102(:,:,:,:)
       real*8,allocatable::S103(:,:,:,:)
       real*8,allocatable::S104(:,:,:,:)
       real*8,allocatable::S105(:,:,:,:)
       real*8,allocatable::S106(:,:,:,:)
       real*8,allocatable::S107(:,:,:,:)
       real*8,allocatable::S108(:,:,:,:)
       real*8,allocatable::S109(:,:,:,:)
       real*8,allocatable::S110(:,:,:,:)
       real*8,allocatable::S111(:,:,:,:)
       real*8,allocatable::S112(:,:,:,:)
       real*8,allocatable::S113(:,:,:,:)
       real*8,allocatable::S114(:,:,:,:)
       real*8,allocatable::S115(:,:,:,:)
       real*8,allocatable::S116(:,:,:,:)
       real*8,allocatable::S117(:,:,:,:)
       real*8,allocatable::S118(:,:,:,:)
       real*8,allocatable::S119(:,:,:,:)
       real*8,allocatable::S120(:,:,:,:)
       real*8,allocatable::S121(:,:,:,:)
       real*8,allocatable::S122(:,:,:,:)
       real*8,allocatable::S123(:,:,:,:)
       real*8,allocatable::S124(:,:,:,:)
       real*8,allocatable::S125(:,:,:,:)
       real*8,allocatable::S126(:,:,:,:)
       real*8,allocatable::S127(:,:,:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::S128(:,:,:,:)
       real*8,allocatable::S129(:,:,:,:)
       real*8,allocatable::S130(:,:,:,:)
       real*8,allocatable::S131(:,:,:,:)
       real*8,allocatable::S132(:,:,:,:)
       real*8,allocatable::S133(:,:,:,:)
       real*8,allocatable::S134(:,:,:,:)
       real*8,allocatable::S136(:,:,:,:)
       real*8,allocatable::S135(:,:,:,:)
       real*8,allocatable::S137(:,:,:,:)
       real*8,allocatable::S139(:,:,:,:)
       real*8,allocatable::S138(:,:,:,:)
       real*8,allocatable::S140(:,:,:,:)
       real*8,allocatable::S144(:,:,:,:)
       real*8,allocatable::S145(:,:,:,:)
       real*8,allocatable::S141(:,:,:,:)
       real*8,allocatable::S142(:,:,:,:)
       real*8,allocatable::S146(:,:,:,:)
       real*8,allocatable::S143(:,:,:,:)
       real*8,allocatable::S147(:,:,:,:)
       real*8,allocatable::S148(:,:,:,:)
       real*8,allocatable::S149(:,:,:,:)
       real*8,allocatable::S150(:,:,:,:)
       real*8,allocatable::S151(:,:,:,:)
       real*8,allocatable::S152(:,:,:,:)
       real*8,allocatable::S153(:,:,:,:)
       real*8,allocatable::S154(:,:,:,:)
       real*8,allocatable::S155(:,:,:,:)
       real*8,allocatable::S156(:,:,:,:)
       real*8,allocatable::S157(:,:,:,:)
       real*8,allocatable::S158(:,:,:,:)
       real*8,allocatable::S159(:,:,:,:)
       real*8,allocatable::S160(:,:,:,:)
       real*8,allocatable::S161(:,:,:,:)
       real*8,allocatable::S162(:,:,:,:)
       real*8,allocatable::S163(:,:,:,:)
       real*8,allocatable::S164(:,:,:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::S86(:,:,:,:)
       real*8,allocatable::S165(:,:,:,:)
       real*8,allocatable::S169(:,:,:,:)
       real*8,allocatable::S166(:,:,:,:)
       real*8,allocatable::S167(:,:,:,:)
       real*8,allocatable::S170(:,:,:,:)
       real*8,allocatable::S168(:,:,:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::S171(:,:,:,:)
       real*8,allocatable::S172(:,:,:,:)
       real*8,allocatable::S173(:,:,:,:)
       real*8,allocatable::S174(:,:,:,:)
       real*8,allocatable::S175(:,:,:,:)
       real*8,allocatable::S179(:,:,:,:)
       real*8,allocatable::S176(:,:,:,:)
       real*8,allocatable::S177(:,:,:,:)
       real*8,allocatable::S180(:,:,:,:)
       real*8,allocatable::S178(:,:,:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q36(:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::Q38(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::S181(:,:,:,:)
       real*8,allocatable::S183(:,:,:,:)
       real*8,allocatable::S184(:,:,:,:)
       real*8,allocatable::S185(:,:,:,:)
       real*8,allocatable::S206(:,:,:,:)
       real*8,allocatable::S207(:,:,:,:)
       real*8,allocatable::S209(:,:,:,:)
       real*8,allocatable::S210(:,:,:,:)
       real*8,allocatable::S211(:,:,:,:)
       real*8,allocatable::S212(:,:,:,:)
       real*8,allocatable::S182(:,:,:,:)
       real*8,allocatable::S208(:,:,:,:)
       real*8,allocatable::Q44(:,:)
       real*8,allocatable::S195(:,:,:,:)
       real*8,allocatable::S193(:,:,:,:)
       real*8,allocatable::S190(:,:,:,:)
       real*8,allocatable::S191(:,:,:,:)
       real*8,allocatable::S194(:,:,:,:)
       real*8,allocatable::S196(:,:,:,:)
       real*8,allocatable::S197(:,:,:,:)
       real*8,allocatable::S198(:,:,:,:)
       real*8,allocatable::S201(:,:,:,:)
       real*8,allocatable::S203(:,:,:,:)
       real*8,allocatable::S202(:,:,:,:)
       real*8,allocatable::Q45(:,:)
       real*8,allocatable::S205(:,:,:,:)
       real*8,allocatable::S204(:,:,:,:)
       real*8,allocatable::S186(:,:,:,:)
       real*8,allocatable::S187(:,:,:,:)
       real*8,allocatable::S188(:,:,:,:)
       real*8,allocatable::S192(:,:,:,:)
       real*8,allocatable::S189(:,:,:,:)
       real*8,allocatable::S199(:,:,:,:)
       real*8,allocatable::S200(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::Z1(:,:,:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::Z2(:,:,:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::Z3(:,:,:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::Z4(:,:,:,:,:,:)
       real*8,allocatable::X5(:,:)
       real*8,allocatable::Z5(:,:,:,:,:,:)
       real*8,allocatable::X6(:,:)
       real*8,allocatable::Z6(:,:,:,:,:,:)
       real*8,allocatable::X7(:,:)
       real*8,allocatable::Z7(:,:,:,:,:,:)
       real*8,allocatable::X8(:,:)
       real*8,allocatable::Z8(:,:,:,:,:,:)
       real*8,allocatable::X9(:,:)
       real*8,allocatable::Z9(:,:,:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::Z10(:,:,:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::Z11(:,:,:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::Z12(:,:,:,:,:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::Z13(:,:,:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::Z14(:,:,:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::Z15(:,:,:,:,:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::Z16(:,:,:,:,:,:)
       real*8,allocatable::X17(:,:,:,:)
       real*8,allocatable::Z17(:,:,:,:,:,:)
       real*8,allocatable::X18(:,:,:,:)
       real*8,allocatable::Z18(:,:,:,:,:,:)
       real*8,allocatable::X19(:,:,:,:)
       real*8,allocatable::Z19(:,:,:,:,:,:)
       real*8,allocatable::X20(:,:,:,:)
       real*8,allocatable::Z20(:,:,:,:,:,:)
       real*8,allocatable::X21(:,:,:,:)
       real*8,allocatable::Z21(:,:,:,:,:,:)
       real*8,allocatable::X22(:,:,:,:)
       real*8,allocatable::Z22(:,:,:,:,:,:)
       real*8,allocatable::X23(:,:,:,:)
       real*8,allocatable::Z23(:,:,:,:,:,:)
       real*8,allocatable::X24(:,:,:,:)
       real*8,allocatable::Z24(:,:,:,:,:,:)
       real*8,allocatable::X25(:,:,:,:)
       real*8,allocatable::Z25(:,:,:,:,:,:)
       real*8,allocatable::X26(:,:,:,:)
       real*8,allocatable::Z26(:,:,:,:,:,:)
       real*8,allocatable::Z27(:,:,:,:,:,:)
       real*8,allocatable::Z28(:,:,:,:,:,:)
       real*8,allocatable::X27(:,:,:,:)
       real*8,allocatable::Z31(:,:,:,:,:,:)
       real*8,allocatable::X28(:,:,:,:)
       real*8,allocatable::Z32(:,:,:,:,:,:)
       real*8,allocatable::X29(:,:,:,:)
       real*8,allocatable::Z33(:,:,:,:,:,:)
       real*8,allocatable::Z42(:,:,:,:,:,:)
       real*8,allocatable::Z43(:,:,:,:,:,:)
       real*8,allocatable::Z44(:,:,:,:,:,:)
       real*8,allocatable::X30(:,:,:,:)
       real*8,allocatable::Z49(:,:,:,:,:,:)
       real*8,allocatable::X31(:,:,:,:)
       real*8,allocatable::Z50(:,:,:,:,:,:)
       real*8,allocatable::Z60(:,:,:,:,:,:)
       real*8,allocatable::Z61(:,:,:,:,:,:)
       real*8,allocatable::Z62(:,:,:,:,:,:)
       real*8,allocatable::Z70(:,:,:,:,:,:)
       real*8,allocatable::Z71(:,:,:,:,:,:)
       real*8,allocatable::Z72(:,:,:,:,:,:)
       real*8,allocatable::Z73(:,:,:,:,:,:)
       real*8,allocatable::Z74(:,:,:,:,:,:)
       real*8,allocatable::Z75(:,:,:,:,:,:)
       real*8,allocatable::Z76(:,:,:,:,:,:)
       real*8,allocatable::Z77(:,:,:,:,:,:)
       real*8,allocatable::Z78(:,:,:,:,:,:)
       real*8,allocatable::Z79(:,:,:,:,:,:)
       real*8,allocatable::Z80(:,:,:,:,:,:)
       real*8,allocatable::Z81(:,:,:,:,:,:)
       real*8,allocatable::X32(:,:,:,:)
       real*8,allocatable::Z91(:,:,:,:,:,:)
       real*8,allocatable::X33(:,:,:,:)
       real*8,allocatable::Z92(:,:,:,:,:,:)
       real*8,allocatable::X34(:,:,:,:)
       real*8,allocatable::Z93(:,:,:,:,:,:)
       real*8,allocatable::X35(:,:,:,:)
       real*8,allocatable::Z94(:,:,:,:,:,:)
       real*8,allocatable::X36(:,:,:,:)
       real*8,allocatable::Z109(:,:,:,:,:,:)
       real*8,allocatable::X37(:,:,:,:)
       real*8,allocatable::Z115(:,:,:,:,:,:)
       real*8,allocatable::X38(:,:,:,:)
       real*8,allocatable::Z121(:,:,:,:,:,:)
       real*8,allocatable::Z129(:,:,:,:,:,:)
       real*8,allocatable::Z130(:,:,:,:,:,:)
       real*8,allocatable::Z134(:,:,:,:,:,:)
       real*8,allocatable::Z212(:,:,:,:,:,:)
       real*8,allocatable::Z141(:,:,:,:,:,:)
       real*8,allocatable::X39(:,:,:,:)
       real*8,allocatable::Z168(:,:,:,:,:,:)
       real*8,allocatable::X40(:,:,:,:)
       real*8,allocatable::Z169(:,:,:,:,:,:)
       real*8,allocatable::X41(:,:,:,:)
       real*8,allocatable::Z170(:,:,:,:,:,:)
       real*8,allocatable::X42(:,:,:,:)
       real*8,allocatable::Z171(:,:,:,:,:,:)
       real*8,allocatable::X43(:,:,:,:)
       real*8,allocatable::Z172(:,:,:,:,:,:)
       real*8,allocatable::X44(:,:,:,:)
       real*8,allocatable::Z173(:,:,:,:,:,:)
C
       allocate(V3A(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       V3A=0.0d0
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHH),size(VAHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M1-N0,M1-N0/),'3412',VAHHHH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S1(M2+1:N3,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,M2+1:N3,M1+1:N1,M1+1:N1))
       X1=0.0d0
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S1)
       deallocate(S1)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHH),size(VAHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M1-N0,M1-N0/),'4312',VAHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S2(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       X2=0.0d0
       call sum_stripe(4,shape(X2),size(X2),'2134',1.000,X2,
     & S2)
       deallocate(S2)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N1,M1-N0/),'1432',VAHPHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S3(M1+1:N1,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X27(N0+1:N1,M2+1:N3,M1+1:N1,M1+1:N1))
       X27=0.0d0
       call sum_stripe(4,shape(X27),size(X27),'3124',1.000,
     & X27,S3)
       deallocate(S3)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,M1-N0/),'1432',VAHPHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S4(M1+1:N1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X28(N0+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       X28=0.0d0
       call sum_stripe(4,shape(X28),size(X28),'3124',1.000,
     & X28,S4)
       deallocate(S4)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N1,M1-N0/),'4132',VAHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S5(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X29(N1+1:N3,M2+1:N3,M2+1:N3,M1+1:N1))
       X29=0.0d0
       call sum_stripe(4,shape(X29),size(X29),'3124',1.000,
     & X29,S5)
       deallocate(S5)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N1,M1-N0/),'4132',VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S6(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N1+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       X4=0.0d0
       call sum_stripe(4,shape(X4),size(X4),'3124',-1.000,
     & X4,S6)
       deallocate(S6)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M1-N0/),'4132',VAHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S7(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',1.000,X4,
     & S7)
       deallocate(S7)
C
       allocate(D1(N1+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAAPPP),size(VAAPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,M2-N1,N1-N1/),'1234',VAAPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S8(M1+1:N1,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'4123',-1.000,
     & X4,S8)
       deallocate(S8)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(Q1(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q1)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X5(N0+1:M1,M1+1:N1))
       X5=0.0d0
       call sum_stripe(2,shape(X5),size(X5),'21',1.000,X5,
     & Q1)
       deallocate(Q1)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,M1-N0/),'12',FAHP,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(Q2(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q2)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X6(M1+1:N1,M1+1:N1))
       X6=0.0d0
       call sum_stripe(2,shape(X6),size(X6),'21',1.000,X6,
     & Q2)
       deallocate(Q2)
C
       allocate(B1(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,M2-N1/),'21',FAHP,B1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q3(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q3)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X7(M2+1:N3,M2+1:N3))
       X7=0.0d0
       call sum_stripe(2,shape(X7),size(X7),'21',-1.000,X7,
     & Q3)
       deallocate(Q3)
C
       allocate(B1(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,N1-N1/),'21',FAHP,B1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q4(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q4)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X8(N1+1:M2,M2+1:N3))
       X8=0.0d0
       call sum_stripe(2,shape(X8),size(X8),'21',-1.000,X8,
     & Q4)
       deallocate(Q4)
C
       allocate(B1(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,N1-N1/),'21',FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q5(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q5)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X9(N1+1:M2,N1+1:M2))
       X9=0.0d0
       call sum_stripe(2,shape(X9),size(X9),'21',-1.000,X9,
     & Q5)
       deallocate(Q5)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M1-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S9(M1+1:N1,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S9),size(S9),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'2314',S9,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z42(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K7*K9*K6*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z42)
       deallocate(D1)
       deallocate(F2)
C
       V3A=V3A+0.500*Z42
       call sum_stripe(6,shape(V3A),size(V3A),'123546',
     & -0.500,V3A,Z42)
       call sum_stripe(6,shape(V3A),size(V3A),'123465',
     & -0.500,V3A,Z42)
       call sum_stripe(6,shape(V3A),size(V3A),'123564',
     & 0.500,V3A,Z42)
       call sum_stripe(6,shape(V3A),size(V3A),'123645',
     & 0.500,V3A,Z42)
       call sum_stripe(6,shape(V3A),size(V3A),'123654',
     & -0.500,V3A,Z42)
       deallocate(Z42)
       deallocate(S9)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,M1-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S10(M1+1:N1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S10),size(S10),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M1-M1,M1-M1/),'2314',S10,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z43(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K7*K9*K6*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z43)
       deallocate(D1)
       deallocate(F2)
C
       V3A=V3A+Z43
       call sum_stripe(6,shape(V3A),size(V3A),'123546',
     & -1.000,V3A,Z43)
       call sum_stripe(6,shape(V3A),size(V3A),'123465',
     & -1.000,V3A,Z43)
       call sum_stripe(6,shape(V3A),size(V3A),'123564',
     & 1.000,V3A,Z43)
       call sum_stripe(6,shape(V3A),size(V3A),'123645',
     & 1.000,V3A,Z43)
       call sum_stripe(6,shape(V3A),size(V3A),'123654',
     & -1.000,V3A,Z43)
       deallocate(Z43)
       deallocate(S10)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,M1-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S11(M1+1:N1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S11),size(S11),shape(D1),size(D1),
     & (/M1-M1,M1-M1,M1-M1,M1-M1/),'2314',S11,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z44(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K7*K9*K6*K6
       I3=K7*K7
       call EGEMM(I1,I2,I3,D1,F2,Z44)
       deallocate(D1)
       deallocate(F2)
C
       V3A=V3A+0.500*Z44
       call sum_stripe(6,shape(V3A),size(V3A),'123546',
     & -0.500,V3A,Z44)
       call sum_stripe(6,shape(V3A),size(V3A),'123465',
     & -0.500,V3A,Z44)
       call sum_stripe(6,shape(V3A),size(V3A),'123564',
     & 0.500,V3A,Z44)
       call sum_stripe(6,shape(V3A),size(V3A),'123645',
     & 0.500,V3A,Z44)
       call sum_stripe(6,shape(V3A),size(V3A),'123654',
     & -0.500,V3A,Z44)
       deallocate(Z44)
       deallocate(S11)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S12(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       X13=0.0d0
       call sum_stripe(4,shape(X13),size(X13),'3124',-1.000,
     & X13,S12)
       deallocate(S12)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S13(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       X14=0.0d0
       call sum_stripe(4,shape(X14),size(X14),'3124',-1.000,
     & X14,S13)
       deallocate(S13)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S14(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N0+1:M1,N1+1:M2,M2+1:N3,M1+1:N1))
       X15=0.0d0
       call sum_stripe(4,shape(X15),size(X15),'3124',-1.000,
     & X15,S14)
       deallocate(S14)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S15(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(M1+1:N1,N1+1:M2,M2+1:N3,M1+1:N1))
       X16=0.0d0
       call sum_stripe(4,shape(X16),size(X16),'3124',-1.000,
     & X16,S15)
       deallocate(S15)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S16(N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X30(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N1))
       X30=0.0d0
       call sum_stripe(4,shape(X30),size(X30),'3124',1.000,
     & X30,S16)
       deallocate(S16)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S17(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X31(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X31=0.0d0
       call sum_stripe(4,shape(X31),size(X31),'3124',1.000,
     & X31,S17)
       deallocate(S17)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'3142',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q6(N0+1:M1,M1+1:N1))
       I1=K7*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       X5=X5+Q6
       deallocate(Q6)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M1-N0/),'3142',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q7(M1+1:N1,M1+1:N1))
       I1=K7*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       X6=X6+Q7
       deallocate(Q7)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N1,M2-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S18(M1+1:N1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X13),size(X13),'4123',-1.000,
     & X13,S18)
       deallocate(S18)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N1,M2-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S19(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X14),size(X14),'4123',-1.000,
     & X14,S19)
       deallocate(S19)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,M2-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S20(M1+1:N1,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X15),size(X15),'4123',-1.000,
     & X15,S20)
       deallocate(S20)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N1-N1,M2-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S21(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X16),size(X16),'4123',-1.000,
     & X16,S21)
       deallocate(S21)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S22(M1+1:N1,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N1))
       X17=0.0d0
       call sum_stripe(4,shape(X17),size(X17),'4123',-1.000,
     & X17,S22)
       deallocate(S22)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N1-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S23(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X18(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X18=0.0d0
       call sum_stripe(4,shape(X18),size(X18),'4123',-1.000,
     & X18,S23)
       deallocate(S23)
C
       allocate(D1(M2+1:N3,M2+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/M2-N1,M2-N1,N0-N0,M2-N1/),'1243',VAHPPP,D1)
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M2-N1,M2-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3A,F2)
       allocate(U59(N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K7*K7*K7*K9
       I3=K6*K6
       call EGEMM(I1,I2,I3,D1,F2,U59)
       deallocate(D1)
       deallocate(F2)
C
       allocate(F1(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(U59),size(U59),shape(F1),size(F1),
     & (/N0-N0,M2-M2,N1-N1,M1-M1,M1-M1,M1-M1/),'561234',U59,F1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Z60(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,F1,B2,Z60)
       deallocate(F1)
       deallocate(B2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'213456',
     & -0.500,V3A,Z60)
       V3A=V3A+0.500*Z60
       deallocate(Z60)
       deallocate(U59)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S24(M2+1:N3,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M2+1:N3,N1+1:M2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(S24),size(S24),shape(D1),size(D1),
     & (/M2-M2,N1-N1,M2-M2,M2-M2/),'2341',S24,D1)
       allocate(F2(M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M2-N1,N1-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z61(N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K7*K7*K7*K9
       I3=K9*K6
       call EGEMM(I1,I2,I3,D1,F2,Z61)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'345612',
     & -1.000,V3A,Z61)
       call sum_stripe(6,shape(V3A),size(V3A),'345621',
     & 1.000,V3A,Z61)
       deallocate(Z61)
       deallocate(S24)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S25(M2+1:N3,N1+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:M2,N1+1:M2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(S25),size(S25),shape(D1),size(D1),
     & (/N1-N1,N1-N1,M2-M2,M2-M2/),'2341',S25,D1)
       allocate(F2(N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N1-N1,N1-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z62(N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K7*K7*K7*K9
       I3=K9*K9
       call EGEMM(I1,I2,I3,D1,F2,Z62)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'345612',
     & -0.500,V3A,Z62)
       call sum_stripe(6,shape(V3A),size(V3A),'345621',
     & 0.500,V3A,Z62)
       deallocate(Z62)
       deallocate(S25)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S26(N1+1:M2,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X21(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       X21=0.0d0
       call sum_stripe(4,shape(X21),size(X21),'4123',-1.000,
     & X21,S26)
       deallocate(S26)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S27(N1+1:M2,N1+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X22(N1+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       X22=0.0d0
       call sum_stripe(4,shape(X22),size(X22),'4123',-1.000,
     & X22,S27)
       deallocate(S27)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q8(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       X7=X7-Q8
       deallocate(Q8)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q9(N1+1:M2,M2+1:N3))
       I1=K6*K9
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       X8=X8-Q9
       deallocate(Q9)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S28(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X21),size(X21),'3124',1.000,
     & X21,S28)
       deallocate(S28)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S29(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X22),size(X22),'3124',1.000,
     & X22,S29)
       deallocate(S29)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q10(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       X9=X9-Q10
       deallocate(Q10)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S30(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S30),size(S30),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,M1-M1/),'2314',S30,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z70(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z70)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & -1.000,V3A,Z70)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & 1.000,V3A,Z70)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & 1.000,V3A,Z70)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & -1.000,V3A,Z70)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & -1.000,V3A,Z70)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & 1.000,V3A,Z70)
       deallocate(Z70)
       deallocate(S30)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S31(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S31),size(S31),shape(D1),size(D1),
     & (/M1-M1,M2-M2,M2-M2,M1-M1/),'2314',S31,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z71(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z71)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & -1.000,V3A,Z71)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & 1.000,V3A,Z71)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & 1.000,V3A,Z71)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & -1.000,V3A,Z71)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & -1.000,V3A,Z71)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & 1.000,V3A,Z71)
       deallocate(Z71)
       deallocate(S31)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S32(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S32),size(S32),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'2314',S32,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z72(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z72)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & -1.000,V3A,Z72)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & 1.000,V3A,Z72)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & 1.000,V3A,Z72)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & -1.000,V3A,Z72)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & -1.000,V3A,Z72)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & 1.000,V3A,Z72)
       deallocate(Z72)
       deallocate(S32)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S33(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S33),size(S33),shape(D1),size(D1),
     & (/M1-M1,N2-N2,M2-M2,M1-M1/),'2314',S33,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z73(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z73)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & -1.000,V3A,Z73)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & 1.000,V3A,Z73)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & 1.000,V3A,Z73)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & -1.000,V3A,Z73)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & -1.000,V3A,Z73)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & 1.000,V3A,Z73)
       deallocate(Z73)
       deallocate(S33)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S34(N1+1:M2,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S34),size(S34),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'2314',S34,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-M2,M2-M2,M1-N0,M1-M1/),'412356',t3B3,F2)
       allocate(Z74(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K7*K6*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z74)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & -1.000,V3A,Z74)
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z74)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z74)
       deallocate(Z74)
       deallocate(S34)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S35(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S35),size(S35),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,M1-M1/),'2314',S35,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-M2,M2-M2,M1-N0,M1-M1/),'412356',t3B3,F2)
       allocate(Z75(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K7*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z75)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & -1.000,V3A,Z75)
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z75)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z75)
       deallocate(Z75)
       deallocate(S35)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N2,M2-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S36(M1+1:N1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S36),size(S36),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,M1-M1/),'2341',S36,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z76(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z76)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z76)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z76)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z76)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z76)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z76)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z76)
       deallocate(Z76)
       deallocate(S36)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N2,M2-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S37(M1+1:N1,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S37),size(S37),shape(D1),size(D1),
     & (/M1-M1,M2-M2,M2-M2,M1-M1/),'2341',S37,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z77(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z77)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z77)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z77)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z77)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z77)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z77)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z77)
       deallocate(Z77)
       deallocate(S37)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,M2-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S38(M1+1:N1,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S38),size(S38),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'2341',S38,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z78(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z78)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z78)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z78)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z78)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z78)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z78)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z78)
       deallocate(Z78)
       deallocate(S38)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,M2-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S39(M1+1:N1,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S39),size(S39),shape(D1),size(D1),
     & (/M1-M1,N2-N2,M2-M2,M1-M1/),'2341',S39,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z79(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z79)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z79)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z79)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z79)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z79)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z79)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z79)
       deallocate(Z79)
       deallocate(S39)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S40(M1+1:N1,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S40),size(S40),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'2341',S40,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-M2,M2-M2,M1-N0,M1-M1/),'412356',t3B3,F2)
       allocate(Z80(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K7*K6*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z80)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z80)
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z80)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z80)
       deallocate(Z80)
       deallocate(S40)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S41(M1+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S41),size(S41),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,M1-M1/),'2341',S41,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-M2,M2-M2,M1-N0,M1-M1/),'412356',t3B3,F2)
       allocate(Z81(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K7*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z81)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z81)
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z81)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z81)
       deallocate(Z81)
       deallocate(S41)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q11(N0+1:M1,M1+1:N1))
       I1=K7*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       X5=X5+Q11
       deallocate(Q11)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q12(M1+1:N1,M1+1:N1))
       I1=K7*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       X6=X6+Q12
       deallocate(Q12)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q13(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
C
       X7=X7+Q13
       deallocate(Q13)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q14(N1+1:M2,M2+1:N3))
       I1=K6*K9
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       X8=X8+Q14
       deallocate(Q14)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q15(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       X9=X9+Q15
       deallocate(Q15)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,M2-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S42(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S42)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S42)
       deallocate(S42)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S43(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S43)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S43)
       deallocate(S43)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S44(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S44)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2314',0.500,X4,
     & S44)
       deallocate(S44)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,M2-N1/),'3412',t2A,D2)
       allocate(S45(M2+1:N3,M2+1:N3,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K6*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S45)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X3(N1+1:N3,M2+1:N3,M2+1:N3,M1+1:N1))
       X3=0.0d0
       call sum_stripe(4,shape(X3),size(X3),'2314',0.500,X3,
     & S45)
       deallocate(S45)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'4132',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S46(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S46)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X32(N0+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       X32=0.0d0
       call sum_stripe(4,shape(X32),size(X32),'2314',1.000,
     & X32,S46)
       deallocate(S46)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'3142',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S47(M2+1:N3,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S47)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X33(N0+1:N1,M2+1:N3,M1+1:N1,M1+1:N1))
       X33=0.0d0
       call sum_stripe(4,shape(X33),size(X33),'2314',1.000,
     & X33,S47)
       deallocate(S47)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M2-N1/),'4213',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S48(N1+1:M2,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S48)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X34(N1+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       X34=0.0d0
       call sum_stripe(4,shape(X34),size(X34),'3412',1.000,
     & X34,S48)
       deallocate(S48)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S49(M2+1:N3,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S49)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X35(N1+1:N3,M2+1:N3,M2+1:N3,M1+1:N1))
       X35=0.0d0
       call sum_stripe(4,shape(X35),size(X35),'3412',1.000,
     & X35,S49)
       deallocate(S49)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,M2-N1/),'1243',VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S50(M1+1:N1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S50)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'3412',0.500,X1,
     & S50)
       deallocate(S50)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4213',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S51(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S51)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X34),size(X34),'2413',-1.000,
     & X34,S51)
       deallocate(S51)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,N1-N1/),'1243',VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S52(M1+1:N1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S52)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'3412',0.500,X2,
     & S52)
       deallocate(S52)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S53(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S53)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X32),size(X32),'2314',-1.000,
     & X32,S53)
       deallocate(S53)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S54(M2+1:N3,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S54)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X33),size(X33),'2314',1.000,
     & X33,S54)
       deallocate(S54)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S55(N1+1:M2,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S55)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X34),size(X34),'3412',1.000,
     & X34,S55)
       deallocate(S55)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S56(M2+1:N3,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S56)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X35),size(X35),'3412',-1.000,
     & X35,S56)
       deallocate(S56)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S57(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S57)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X34),size(X34),'2413',-1.000,
     & X34,S57)
       deallocate(S57)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S58(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S58)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X34),size(X34),'2341',0.500,
     & X34,S58)
       deallocate(S58)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S59(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S59)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X34),size(X34),'2341',1.000,
     & X34,S59)
       deallocate(S59)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S60(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S60)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X34),size(X34),'2341',0.500,
     & X34,S60)
       deallocate(S60)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,M2-N1,N1-N1,M1-M1/),'452136',t3A,F2)
       allocate(S61(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S61)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X34),size(X34),'2341',-0.500,
     & X34,S61)
       deallocate(S61)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,M2-N1,N1-N1,M1-M1/),'452136',t3A,F2)
       allocate(S62(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S62)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X34),size(X34),'2341',-1.000,
     & X34,S62)
       deallocate(S62)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,M2-N1,N1-N1,M1-M1/),'452136',t3A,F2)
       allocate(S63(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S63)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X34),size(X34),'2341',-0.500,
     & X34,S63)
       deallocate(S63)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,M2-N1,M2-N1,M1-M1/),'453126',t3A,F2)
       allocate(S64(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K6*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S64)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X36(N1+1:N3,M2+1:N3,M2+1:N3,M1+1:N1))
       X36=0.0d0
       call sum_stripe(4,shape(X36),size(X36),'2341',1.000,
     & X36,S64)
       deallocate(S64)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,M2-N1,M2-N1,M1-M1/),'453126',t3A,F2)
       allocate(S65(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K6*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S65)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X36),size(X36),'2341',2.000,
     & X36,S65)
       deallocate(S65)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,M2-N1,M2-N1,M1-M1/),'453126',t3A,F2)
       allocate(S66(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K6*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S66)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X36),size(X36),'2341',1.000,
     & X36,S66)
       deallocate(S66)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S67(M1+1:N1,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S67)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X10(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N1))
       X10=0.0d0
       call sum_stripe(4,shape(X10),size(X10),'3412',0.500,
     & X10,S67)
       deallocate(S67)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S68(M1+1:N1,M1+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S68)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X11(N0+1:M1,M1+1:N1,M1+1:N1,M1+1:N1))
       X11=0.0d0
       call sum_stripe(4,shape(X11),size(X11),'3412',0.500,
     & X11,S68)
       deallocate(S68)
C
       allocate(D1(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S69(M1+1:N1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S69)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X12(M1+1:N1,M1+1:N1,M1+1:N1,M1+1:N1))
       X12=0.0d0
       call sum_stripe(4,shape(X12),size(X12),'3412',0.500,
     & X12,S69)
       deallocate(S69)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,M2-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(S70(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S70)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X37(N0+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       X37=0.0d0
       call sum_stripe(4,shape(X37),size(X37),'2341',1.000,
     & X37,S70)
       deallocate(S70)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,M2-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(S71(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S71)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X37),size(X37),'2341',1.000,
     & X37,S71)
       deallocate(S71)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(S72(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S72)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X37),size(X37),'2341',2.000,
     & X37,S72)
       deallocate(S72)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(S73(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S73)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X37),size(X37),'2341',2.000,
     & X37,S73)
       deallocate(S73)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(S74(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S74)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X37),size(X37),'2341',1.000,
     & X37,S74)
       deallocate(S74)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(S75(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S75)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X37),size(X37),'2341',1.000,
     & X37,S75)
       deallocate(S75)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N1-N1,M2-N1,M1-N0,M1-M1/),'413256',t3A,F2)
       allocate(S76(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S76)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X38(N0+1:N1,M2+1:N3,M1+1:N1,M1+1:N1))
       X38=0.0d0
       call sum_stripe(4,shape(X38),size(X38),'2341',1.000,
     & X38,S76)
       deallocate(S76)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N1-N1,M2-N1,M1-N0,M1-M1/),'413256',t3A,F2)
       allocate(S77(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S77)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X38),size(X38),'2341',1.000,
     & X38,S77)
       deallocate(S77)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N1-N1,M2-N1,M1-N0,M1-M1/),'423156',t3A,F2)
       allocate(S78(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S78)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X38),size(X38),'2341',-0.500,
     & X38,S78)
       deallocate(S78)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N1-N1,M2-N1,M1-N0,M1-M1/),'423156',t3A,F2)
       allocate(S79(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S79)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X38),size(X38),'2341',-0.500,
     & X38,S79)
       deallocate(S79)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S80(M2+1:N3,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S80)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'3412',-1.000,
     & X13,S80)
       deallocate(S80)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S81(M2+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S81)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'3412',-1.000,
     & X14,S81)
       deallocate(S81)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S82(M2+1:N3,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S82)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X15),size(X15),'3412',-1.000,
     & X15,S82)
       deallocate(S82)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S83(M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S83)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X16),size(X16),'3412',-1.000,
     & X16,S83)
       deallocate(S83)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S84(N1+1:M2,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S84)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S84),size(S84),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'3412',S84,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N1,M2-N1,M1-N0,M1-M1/),'431256',t3A,F2)
       allocate(Z129(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K7*K6*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z129)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z129)
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z129)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z129)
       deallocate(Z129)
       deallocate(S84)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S85(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S85)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S85),size(S85),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N1-N1,M1-M1/),'3412',S85,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N1,M2-N1,M1-N0,M1-M1/),'431256',t3A,F2)
       allocate(Z130(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K7*K6*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z130)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z130)
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z130)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z130)
       deallocate(Z130)
       deallocate(S85)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Q16(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q16)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X5),size(X5),'21',0.500,X5,
     & Q16)
       deallocate(Q16)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Q17(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q17)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X6),size(X6),'21',0.500,X6,
     & Q17)
       deallocate(Q17)
C
       allocate(D1(M2+1:N3,M2+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M2-N1,M2-N1,N0-N0,N0-N0/),'1234',VAHHPP,D1)
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M2-N1,M2-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3A,F2)
       allocate(U133(N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K7*K7*K7*K9
       I3=K6*K6
       call EGEMM(I1,I2,I3,D1,F2,U133)
       deallocate(D1)
       deallocate(F2)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(U133),size(U133),shape(F1),size(F1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1,M1-M1,M1-M1/),'561234',U133,F1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,M2-N1/),'3412',t2A,D2)
       allocate(Z134(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7*K9
       I2=K6*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,F1,D2,Z134)
       deallocate(F1)
       deallocate(D2)
C
       V3A=V3A+0.250*Z134
       deallocate(Z134)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(U133),size(U133),shape(F1),size(F1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1,M1-M1,M1-M1/),'651234',U133,F1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(U211(M2+1:N3,N0+1:N1,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7*K9*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,F1,B2,U211)
       deallocate(F1)
       deallocate(B2)
C
       allocate(F1(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(U211),size(U211),shape(F1),size(F1),
     & (/N0-N0,M2-M2,N1-N1,M1-M1,M1-M1,M1-M1/),'213456',U211,F1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Z212(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,F1,B2,Z212)
       deallocate(F1)
       deallocate(B2)
C
       V3A=V3A+0.500*Z212
       deallocate(Z212)
       deallocate(U211)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,M2-N1/),'3412',t2A,D2)
       allocate(S87(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9
       I2=K6*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S87)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X20(N1+1:M2,N1+1:M2,M2+1:N3,M2+1:N3))
       X20=0.0d0
       call sum_stripe(4,shape(X20),size(X20),'3412',0.500,
     & X20,S87)
       deallocate(S87)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S88(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S88)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X21),size(X21),'3412',0.500,
     & X21,S88)
       deallocate(S88)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S89(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S89)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X22),size(X22),'3412',0.500,
     & X22,S89)
       deallocate(S89)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,M2-N1/),'3412',t2A,D2)
       allocate(Q18(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q18)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X7),size(X7),'21',-0.500,X7,
     & Q18)
       deallocate(Q18)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,M2-N1/),'3412',t2A,D2)
       allocate(Q19(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q19)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X8),size(X8),'21',-0.500,X8,
     & Q19)
       deallocate(Q19)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(Q20(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q20)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N1+1:M2,N1+1:M2))
       call reorder_shift(2,shape(Q20),size(Q20),shape(B1),size(B1),
     & (/N1-N1,N1-N1/),'21',Q20,B1)
       allocate(F2(N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N1-N1,M2-N1,M2-N1,M1-N0,M1-N0,M1-M1/),'312456',t3A,F2)
       allocate(Z141(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K7*K6*K6
       I3=K9
       call EGEMM(I1,I2,I3,B1,F2,Z141)
       deallocate(B1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124563',
     & 0.500,V3A,Z141)
       deallocate(Z141)
       deallocate(Q20)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S90(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S90)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X34),size(X34),'2341',-1.000,
     & X34,S90)
       deallocate(S90)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S91(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S91)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X34),size(X34),'2341',-1.000,
     & X34,S91)
       deallocate(S91)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S92(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S92)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X34),size(X34),'2341',-1.000,
     & X34,S92)
       deallocate(S92)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S93(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S93)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X34),size(X34),'2341',-1.000,
     & X34,S93)
       deallocate(S93)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S94(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S94)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X34),size(X34),'2341',-1.000,
     & X34,S94)
       deallocate(S94)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S95(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S95)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X34),size(X34),'2341',-1.000,
     & X34,S95)
       deallocate(S95)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S96(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S96)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X34),size(X34),'2341',-1.000,
     & X34,S96)
       deallocate(S96)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S97(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S97)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X34),size(X34),'2341',-1.000,
     & X34,S97)
       deallocate(S97)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,M2-M2,M2-M2,M1-M1/),'451236',t3B3,F2)
       allocate(S98(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K6*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S98)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X36),size(X36),'2341',2.000,
     & X36,S98)
       deallocate(S98)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N2-N2,M2-M2,M2-M2,M1-M1/),'451236',t3B3,F2)
       allocate(S99(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K6*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S99)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X36),size(X36),'2341',2.000,
     & X36,S99)
       deallocate(S99)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,M2-M2,M2-M2,M1-M1/),'451236',t3B3,F2)
       allocate(S100(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K6*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S100)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X36),size(X36),'2341',2.000,
     & X36,S100)
       deallocate(S100)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,M2-M2,M2-M2,M1-M1/),'451236',t3B3,F2)
       allocate(S101(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K6*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S101)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X36),size(X36),'2341',2.000,
     & X36,S101)
       deallocate(S101)
C
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(Z109(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K6
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,X36,D2,Z109)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'356124',
     & 0.500,V3A,Z109)
       call sum_stripe(6,shape(V3A),size(V3A),'346125',
     & -0.500,V3A,Z109)
       call sum_stripe(6,shape(V3A),size(V3A),'345126',
     & 0.500,V3A,Z109)
       deallocate(Z109)
       deallocate(X36)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S102(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S102)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X37),size(X37),'2341',-2.000,
     & X37,S102)
       deallocate(S102)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S103(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S103)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X37),size(X37),'2341',-2.000,
     & X37,S103)
       deallocate(S103)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S104(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S104)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X37),size(X37),'2341',-2.000,
     & X37,S104)
       deallocate(S104)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S105(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S105)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X37),size(X37),'2341',-2.000,
     & X37,S105)
       deallocate(S105)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S106(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S106)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X37),size(X37),'2341',-2.000,
     & X37,S106)
       deallocate(S106)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S107(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S107)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X37),size(X37),'2341',-2.000,
     & X37,S107)
       deallocate(S107)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S108(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S108)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X37),size(X37),'2341',-2.000,
     & X37,S108)
       deallocate(S108)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S109(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S109)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X37),size(X37),'2341',-2.000,
     & X37,S109)
       deallocate(S109)
C
       allocate(D2(N0+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z115(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K9
       I2=K7*K6*K6
       I3=K1
       call EGEMM(I1,I2,I3,X37,D2,Z115)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'126345',
     & -0.500,V3A,Z115)
       call sum_stripe(6,shape(V3A),size(V3A),'125346',
     & 0.500,V3A,Z115)
       call sum_stripe(6,shape(V3A),size(V3A),'124356',
     & -0.500,V3A,Z115)
       deallocate(Z115)
       deallocate(X37)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-M2,M2-M2,M1-N0,M1-M1/),'412356',t3B3,F2)
       allocate(S110(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S110)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X38),size(X38),'2341',-1.000,
     & X38,S110)
       deallocate(S110)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-M2,M2-M2,M1-N0,M1-M1/),'412356',t3B3,F2)
       allocate(S111(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S111)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X38),size(X38),'2341',-1.000,
     & X38,S111)
       deallocate(S111)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,M2-N1,M1-N0,M1-M1/),'413256',t3B1,F2)
       allocate(S112(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S112)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X38),size(X38),'2341',1.000,
     & X38,S112)
       deallocate(S112)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,M2-N1,M1-N0,M1-M1/),'413256',t3B1,F2)
       allocate(S113(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S113)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X38),size(X38),'2341',1.000,
     & X38,S113)
       deallocate(S113)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,M2-N1,M1-N0,M1-M1/),'413256',t3B1,F2)
       allocate(S114(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S114)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X38),size(X38),'2341',1.000,
     & X38,S114)
       deallocate(S114)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,M2-N1,M1-N0,M1-M1/),'413256',t3B1,F2)
       allocate(S115(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S115)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X38),size(X38),'2341',1.000,
     & X38,S115)
       deallocate(S115)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z121(M2+1:N3,N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1,M1+1:N1))
       I1=K7*K7*K6
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X38,D2,Z121)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'136245',
     & 1.000,V3A,Z121)
       call sum_stripe(6,shape(V3A),size(V3A),'236145',
     & -1.000,V3A,Z121)
       call sum_stripe(6,shape(V3A),size(V3A),'135246',
     & -1.000,V3A,Z121)
       call sum_stripe(6,shape(V3A),size(V3A),'235146',
     & 1.000,V3A,Z121)
       call sum_stripe(6,shape(V3A),size(V3A),'134256',
     & 1.000,V3A,Z121)
       call sum_stripe(6,shape(V3A),size(V3A),'234156',
     & -1.000,V3A,Z121)
       deallocate(Z121)
       deallocate(X38)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S116(M2+1:N3,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S116)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X39(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       X39=0.0d0
       call sum_stripe(4,shape(X39),size(X39),'3412',1.000,
     & X39,S116)
       deallocate(S116)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S117(M2+1:N3,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S117)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X40(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       X40=0.0d0
       call sum_stripe(4,shape(X40),size(X40),'3412',1.000,
     & X40,S117)
       deallocate(S117)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S118(M2+1:N3,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S118)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X41(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       X41=0.0d0
       call sum_stripe(4,shape(X41),size(X41),'3412',1.000,
     & X41,S118)
       deallocate(S118)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S119(M2+1:N3,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S119)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X42(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       X42=0.0d0
       call sum_stripe(4,shape(X42),size(X42),'3412',1.000,
     & X42,S119)
       deallocate(S119)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S120(N1+1:M2,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S120)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X43(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       X43=0.0d0
       call sum_stripe(4,shape(X43),size(X43),'3412',1.000,
     & X43,S120)
       deallocate(S120)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S121(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S121)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X44(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       X44=0.0d0
       call sum_stripe(4,shape(X44),size(X44),'3412',1.000,
     & X44,S121)
       deallocate(S121)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S122(M2+1:N3,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S122)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'3412',-1.000,
     & X13,S122)
       deallocate(S122)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S123(M2+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S123)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'3412',-1.000,
     & X14,S123)
       deallocate(S123)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S124(M2+1:N3,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S124)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X15),size(X15),'3412',-1.000,
     & X15,S124)
       deallocate(S124)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S125(M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S125)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X16),size(X16),'3412',-1.000,
     & X16,S125)
       deallocate(S125)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S126(N1+1:M2,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S126)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X17),size(X17),'3412',-1.000,
     & X17,S126)
       deallocate(S126)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,M1,N1,X17,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N1,M2-N1,M1-N0,M1-M1/),'431256',t3A,F2)
       allocate(Z17(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K7*K6*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X17,F2,Z17)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & -1.000,V3A,Z17)
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z17)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z17)
       deallocate(Z17)
       deallocate(X17)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S127(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S127)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X18),size(X18),'3412',-1.000,
     & X18,S127)
       deallocate(S127)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,M1,N1,X18,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N1,M2-N1,M1-N0,M1-M1/),'431256',t3A,F2)
       allocate(Z18(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K7*K6*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X18,F2,Z18)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & -1.000,V3A,Z18)
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z18)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z18)
       deallocate(Z18)
       deallocate(X18)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(Q21(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q21)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X5),size(X5),'21',1.000,X5,
     & Q21)
       deallocate(Q21)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(Q22(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q22)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X6),size(X6),'21',1.000,X6,
     & Q22)
       deallocate(Q22)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',t2B,D2)
       allocate(Q23(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q23)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X7),size(X7),'21',-1.000,X7,
     & Q23)
       deallocate(Q23)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',t2B,D2)
       allocate(Q24(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q24)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X8),size(X8),'21',-1.000,X8,
     & Q24)
       deallocate(Q24)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(Q25(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q25)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X9),size(X9),'21',-1.000,X9,
     & Q25)
       deallocate(Q25)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S128(M2+1:N3,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S128)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X23(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       X23=0.0d0
       call sum_stripe(4,shape(X23),size(X23),'3412',1.000,
     & X23,S128)
       deallocate(S128)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,M1,N1,X23,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z23(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X23,F2,Z23)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z23)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z23)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z23)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z23)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z23)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z23)
       deallocate(Z23)
       deallocate(X23)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S129(M2+1:N3,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S129)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X24(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       X24=0.0d0
       call sum_stripe(4,shape(X24),size(X24),'3412',1.000,
     & X24,S129)
       deallocate(S129)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,M2,N3,M1,N1,X24,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z24(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X24,F2,Z24)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z24)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z24)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z24)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z24)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z24)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z24)
       deallocate(Z24)
       deallocate(X24)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S130(M2+1:N3,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S130)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X25(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       X25=0.0d0
       call sum_stripe(4,shape(X25),size(X25),'3412',1.000,
     & X25,S130)
       deallocate(S130)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,M2,N3,M1,N1,X25,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z25(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X25,F2,Z25)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z25)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z25)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z25)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z25)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z25)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z25)
       deallocate(Z25)
       deallocate(X25)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S131(M2+1:N3,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S131)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X26(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       X26=0.0d0
       call sum_stripe(4,shape(X26),size(X26),'3412',1.000,
     & X26,S131)
       deallocate(S131)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,M2,N3,M1,N1,X26,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z26(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X26,F2,Z26)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z26)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z26)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z26)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z26)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z26)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z26)
       deallocate(Z26)
       deallocate(X26)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S132(N1+1:M2,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S132)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X43),size(X43),'3412',1.000,
     & X43,S132)
       deallocate(S132)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S133(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S133)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X44),size(X44),'3412',1.000,
     & X44,S133)
       deallocate(S133)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M1-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S134(M1+1:N1,N0+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S134)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S134),size(S134),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3214',S134,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S136(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S136)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X28),size(X28),'2134',1.000,
     & X28,S136)
       deallocate(S136)
C
       allocate(D2(N0+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z32(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K9
       I2=K7*K6*K6
       I3=K1
       call EGEMM(I1,I2,I3,X28,D2,Z32)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124356',
     & -1.000,V3A,Z32)
       call sum_stripe(6,shape(V3A),size(V3A),'125346',
     & 1.000,V3A,Z32)
       call sum_stripe(6,shape(V3A),size(V3A),'124365',
     & 1.000,V3A,Z32)
       call sum_stripe(6,shape(V3A),size(V3A),'125364',
     & -1.000,V3A,Z32)
       call sum_stripe(6,shape(V3A),size(V3A),'126345',
     & -1.000,V3A,Z32)
       call sum_stripe(6,shape(V3A),size(V3A),'126354',
     & 1.000,V3A,Z32)
       deallocate(Z32)
       deallocate(X28)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S134),size(S134),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'2314',S134,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S135(M2+1:N3,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S135)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X27),size(X27),'2134',-1.000,
     & X27,S135)
       deallocate(S135)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z31(M2+1:N3,N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1,M1+1:N1))
       I1=K7*K7*K6
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X27,D2,Z31)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234156',
     & -1.000,V3A,Z31)
       call sum_stripe(6,shape(V3A),size(V3A),'134256',
     & 1.000,V3A,Z31)
       call sum_stripe(6,shape(V3A),size(V3A),'235146',
     & 1.000,V3A,Z31)
       call sum_stripe(6,shape(V3A),size(V3A),'135246',
     & -1.000,V3A,Z31)
       call sum_stripe(6,shape(V3A),size(V3A),'234165',
     & 1.000,V3A,Z31)
       call sum_stripe(6,shape(V3A),size(V3A),'134265',
     & -1.000,V3A,Z31)
       call sum_stripe(6,shape(V3A),size(V3A),'235164',
     & -1.000,V3A,Z31)
       call sum_stripe(6,shape(V3A),size(V3A),'135264',
     & 1.000,V3A,Z31)
       call sum_stripe(6,shape(V3A),size(V3A),'236145',
     & -1.000,V3A,Z31)
       call sum_stripe(6,shape(V3A),size(V3A),'136245',
     & 1.000,V3A,Z31)
       call sum_stripe(6,shape(V3A),size(V3A),'236154',
     & 1.000,V3A,Z31)
       call sum_stripe(6,shape(V3A),size(V3A),'136254',
     & -1.000,V3A,Z31)
       deallocate(Z31)
       deallocate(X27)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S137(M2+1:N3,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S137)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S137),size(S137),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'2314',S137,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S139(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S139)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'3124',-1.000,
     & X4,S139)
       deallocate(S139)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S137),size(S137),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'2314',S137,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S138(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S138)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',1.000,X3,
     & S138)
       deallocate(S138)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,M2-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S140(M1+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S140)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S140),size(S140),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'2341',S140,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S144(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S144)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X29),size(X29),'3124',1.000,
     & X29,S144)
       deallocate(S144)
C
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(Z33(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K6
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,X29,D2,Z33)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'345126',
     & 1.000,V3A,Z33)
       call sum_stripe(6,shape(V3A),size(V3A),'345216',
     & -1.000,V3A,Z33)
       call sum_stripe(6,shape(V3A),size(V3A),'346125',
     & -1.000,V3A,Z33)
       call sum_stripe(6,shape(V3A),size(V3A),'346215',
     & 1.000,V3A,Z33)
       call sum_stripe(6,shape(V3A),size(V3A),'356124',
     & 1.000,V3A,Z33)
       call sum_stripe(6,shape(V3A),size(V3A),'356214',
     & -1.000,V3A,Z33)
       deallocate(Z33)
       deallocate(X29)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S140),size(S140),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'2341',S140,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S145(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S145)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'3124',-1.000,
     & X4,S145)
       deallocate(S145)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S140),size(S140),shape(D1),size(D1),
     & (/N1-N1,N0-N0,M2-M2,M1-M1/),'3241',S140,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S141(M1+1:N1,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S141)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'3124',1.000,X1,
     & S141)
       deallocate(S141)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N1-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S142(M1+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S142)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S142),size(S142),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2341',S142,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S146(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S146)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',1.000,X4,
     & S146)
       deallocate(S146)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S142),size(S142),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N1-N1,M1-M1/),'3241',S142,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S143(M1+1:N1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S143)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'3124',1.000,X2,
     & S143)
       deallocate(S143)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S147(M1+1:N1,N0+1:M1,N0+1:M1,N1+1:N3))
       I1=K3*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S147)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S147),size(S147),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N0-N0,M1-M1/),'4231',S147,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S148(M1+1:N1,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S148)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'3124',1.000,
     & X10,S148)
       deallocate(S148)
C
       call sumx_sorted3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,M1,N0,M1,M1,N1,M1,N1,X10,VAHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z10(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K7*K9*K6*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,X10,F2,Z10)
       deallocate(F2)
C
       V3A=V3A+0.500*Z10
       call sum_stripe(6,shape(V3A),size(V3A),'123546',
     & -0.500,V3A,Z10)
       call sum_stripe(6,shape(V3A),size(V3A),'123645',
     & 0.500,V3A,Z10)
       deallocate(Z10)
       deallocate(X10)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S149(M1+1:N1,N0+1:M1,M1+1:N1,N1+1:N3))
       I1=K3*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S149)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S149),size(S149),shape(D1),size(D1),
     & (/N1-N1,N0-N0,M1-M1,M1-M1/),'4231',S149,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S150(M1+1:N1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S150)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'3124',1.000,
     & X11,S150)
       deallocate(S150)
C
       call sumx_sorted3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,M1,N1,M1,N1,X11,VAHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z11(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K7*K9*K6*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,X11,F2,Z11)
       deallocate(F2)
C
       V3A=V3A+Z11
       call sum_stripe(6,shape(V3A),size(V3A),'123546',
     & -1.000,V3A,Z11)
       call sum_stripe(6,shape(V3A),size(V3A),'123645',
     & 1.000,V3A,Z11)
       deallocate(Z11)
       deallocate(X11)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S151(M1+1:N1,M1+1:N1,M1+1:N1,N1+1:N3))
       I1=K3*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S151)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S151),size(S151),shape(D1),size(D1),
     & (/N1-N1,M1-M1,M1-M1,M1-M1/),'4231',S151,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S152(M1+1:N1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S152)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'3124',1.000,
     & X12,S152)
       deallocate(S152)
C
       call sumx_sorted3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,M1,N1,M1,N1,X12,VAHHHH, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z12(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K7*K9*K6*K6
       I3=K7*K7
       call EGEMM(I1,I2,I3,X12,F2,Z12)
       deallocate(F2)
C
       V3A=V3A+0.500*Z12
       call sum_stripe(6,shape(V3A),size(V3A),'123546',
     & -0.500,V3A,Z12)
       call sum_stripe(6,shape(V3A),size(V3A),'123645',
     & 0.500,V3A,Z12)
       deallocate(Z12)
       deallocate(X12)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M2-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S153(M1+1:N1,N0+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S153)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S153),size(S153),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,M1-M1/),'2341',S153,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S154(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S154)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X13),size(X13),'3124',1.000,
     & X13,S154)
       deallocate(S154)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,M1,N1,X13,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(Z13(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X13,F2,Z13)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & -1.000,V3A,Z13)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & 1.000,V3A,Z13)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & 1.000,V3A,Z13)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & -1.000,V3A,Z13)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & -1.000,V3A,Z13)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & 1.000,V3A,Z13)
       deallocate(Z13)
       deallocate(X13)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,M2-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S155(M1+1:N1,N0+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S155)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S155),size(S155),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S155,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S156(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S156)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X14),size(X14),'3124',1.000,
     & X14,S156)
       deallocate(S156)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M1,N1,X14,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(Z14(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X14,F2,Z14)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & -1.000,V3A,Z14)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & 1.000,V3A,Z14)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & 1.000,V3A,Z14)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & -1.000,V3A,Z14)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & -1.000,V3A,Z14)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & 1.000,V3A,Z14)
       deallocate(Z14)
       deallocate(X14)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S157(M1+1:N1,N0+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S157)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S157),size(S157),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'2341',S157,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S158(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S158)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X15),size(X15),'3124',1.000,
     & X15,S158)
       deallocate(S158)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,M2,N3,M1,N1,X15,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N1,N1-N1,M1-N0,M1-M1/),'421356',t3A,F2)
       allocate(Z15(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X15,F2,Z15)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z15)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z15)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z15)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z15)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z15)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z15)
       deallocate(Z15)
       deallocate(X15)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S159(M1+1:N1,N0+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S159)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S159),size(S159),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'2341',S159,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S160(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S160)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X16),size(X16),'3124',1.000,
     & X16,S160)
       deallocate(S160)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,M1,N1,X16,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N1,N1-N1,M1-N0,M1-M1/),'421356',t3A,F2)
       allocate(Z16(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X16,F2,Z16)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z16)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z16)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z16)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z16)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z16)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z16)
       deallocate(Z16)
       deallocate(X16)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S161(M1+1:N1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S161)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S161),size(S161),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'3241',S161,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S162(N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S162)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X30),size(X30),'3124',-1.000,
     & X30,S162)
       deallocate(S162)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N1,M2-N1,M1-N0,M1-M1/),'431256',t3A,F2)
       allocate(Z49(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K7*K6*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X30,F2,Z49)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & -1.000,V3A,Z49)
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z49)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z49)
       deallocate(Z49)
       deallocate(X30)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S163(M1+1:N1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S163)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S163),size(S163),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'3241',S163,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S164(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S164)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X31),size(X31),'3124',-1.000,
     & X31,S164)
       deallocate(S164)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N1,M2-N1,M1-N0,M1-M1/),'431256',t3A,F2)
       allocate(Z50(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K7*K6*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X31,F2,Z50)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & -1.000,V3A,Z50)
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z50)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z50)
       deallocate(Z50)
       deallocate(X31)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q26(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q26),size(Q26),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q26,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(Q27(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q27)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X5),size(X5),'21',1.000,X5,
     & Q27)
       deallocate(Q27)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q28(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q28)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(Q28),size(Q28),shape(B1),size(B1),
     & (/N1-N1,M1-M1/),'21',Q28,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(Q29(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q29)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X6),size(X6),'21',1.000,X6,
     & Q29)
       deallocate(Q29)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,M2-N1/),'3412',t2A,D2)
       allocate(S86(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K6*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S86)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X19(M2+1:N3,N1+1:M2,M2+1:N3,M2+1:N3))
       X19=0.0d0
       call sum_stripe(4,shape(X19),size(X19),'3412',0.500,
     & X19,S86)
       deallocate(S86)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S165(M2+1:N3,N0+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S165)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(S165),size(S165),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,M2-M2/),'2341',S165,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S169(N1+1:M2,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S169)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X21),size(X21),'4123',-1.000,
     & X21,S169)
       deallocate(S169)
C
       call sumx_sorted1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & M2,N3,N1,M2,M2,N3,N1,M2,X21,VAAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M2-N1,N1-N1,M2-N1,M1-N0,M1-N0,M1-M1/),'132456',t3A,F2)
       allocate(Z21(M2+1:N3,M1+1:N1,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K7*K7*K6
       I3=K9*K6
       call EGEMM(I1,I2,I3,X21,F2,Z21)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'245613',
     & 1.000,V3A,Z21)
       call sum_stripe(6,shape(V3A),size(V3A),'145623',
     & -1.000,V3A,Z21)
       deallocate(Z21)
       deallocate(X21)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(S165),size(S165),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,M2-M2/),'2341',S165,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S166(M2+1:N3,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S166)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X19),size(X19),'3124',1.000,
     & X19,S166)
       deallocate(S166)
C
       call sumx_sorted3412(N1,N3,N1,N3,N1,N3,N1,M2,
     & M2,N3,N1,M2,M2,N3,M2,N3,X19,VAAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M2-N1,N1-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z19(N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K7*K7*K7*K9
       I3=K9*K6
       call EGEMM(I1,I2,I3,X19,F2,Z19)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'345612',
     & 1.000,V3A,Z19)
       deallocate(Z19)
       deallocate(X19)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S167(M2+1:N3,N0+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S167)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(S167),size(S167),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M2-M2/),'2341',S167,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S170(N1+1:M2,N1+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S170)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X22),size(X22),'4123',-1.000,
     & X22,S170)
       deallocate(S170)
C
       call sumx_sorted1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,M2,N1,M2,M2,N3,N1,M2,X22,VAAPPP, 1.000)
C
       allocate(F2(N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N1-N1,N1-N1,M2-N1,M1-N0,M1-N0,M1-M1/),'231456',t3A,F2)
       allocate(Z22(M2+1:N3,M1+1:N1,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K7*K7*K6
       I3=K9*K9
       call EGEMM(I1,I2,I3,X22,F2,Z22)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'245613',
     & -0.500,V3A,Z22)
       call sum_stripe(6,shape(V3A),size(V3A),'145623',
     & 0.500,V3A,Z22)
       deallocate(Z22)
       deallocate(X22)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(S167),size(S167),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M2-M2/),'2341',S167,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S168(M2+1:N3,N1+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S168)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X20),size(X20),'3124',1.000,
     & X20,S168)
       deallocate(S168)
C
       call sumx_sorted3412(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,M2,N1,M2,M2,N3,M2,N3,X20,VAAPPP, 1.000)
C
       allocate(F2(N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N1-N1,N1-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z20(N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K7*K7*K7*K9
       I3=K9*K9
       call EGEMM(I1,I2,I3,X20,F2,Z20)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'345612',
     & 0.500,V3A,Z20)
       deallocate(Z20)
       deallocate(X20)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q30(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q31(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q30,B2,Q31)
       deallocate(B2)
C
       call sum_stripe(2,shape(X7),size(X7),'21',-1.000,X7,
     & Q31)
       deallocate(Q31)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q32(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q32)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q34(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q32,B2,Q34)
       deallocate(B2)
C
       call sum_stripe(2,shape(X9),size(X9),'21',-1.000,X9,
     & Q34)
       deallocate(Q34)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q33(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q32,B2,Q33)
       deallocate(B2)
C
       call sum_stripe(2,shape(X8),size(X8),'21',-1.000,X8,
     & Q33)
       deallocate(Q33)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S171(M1+1:N1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S171)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S171),size(S171),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,M1-M1/),'3241',S171,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S172(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S172)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X39),size(X39),'3124',-1.000,
     & X39,S172)
       deallocate(S172)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z168(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X39,F2,Z168)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z168)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z168)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z168)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z168)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z168)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z168)
       deallocate(Z168)
       deallocate(X39)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S173(M1+1:N1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S173)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S173),size(S173),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'3241',S173,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S174(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S174)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X40),size(X40),'3124',-1.000,
     & X40,S174)
       deallocate(S174)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z169(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X40,F2,Z169)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z169)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z169)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z169)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z169)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z169)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z169)
       deallocate(Z169)
       deallocate(X40)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S175(M1+1:N1,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S175)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S175),size(S175),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'3241',S175,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S179(N1+1:M2,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S179)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X43),size(X43),'3124',-1.000,
     & X43,S179)
       deallocate(S179)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-M2,M2-M2,M1-N0,M1-M1/),'412356',t3B3,F2)
       allocate(Z172(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K7*K6*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X43,F2,Z172)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z172)
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z172)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z172)
       deallocate(Z172)
       deallocate(X43)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S175),size(S175),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'3241',S175,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S176(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S176)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X41),size(X41),'3124',-1.000,
     & X41,S176)
       deallocate(S176)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z170(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X41,F2,Z170)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z170)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z170)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z170)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z170)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z170)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z170)
       deallocate(Z170)
       deallocate(X41)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S177(M1+1:N1,M1+1:N2,N0+1:N1,N2+1:M2))
       I1=K0*K1*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S177)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S177),size(S177),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'3241',S177,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S180(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S180)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X44),size(X44),'3124',-1.000,
     & X44,S180)
       deallocate(S180)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-M2,M2-M2,M1-N0,M1-M1/),'412356',t3B3,F2)
       allocate(Z173(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K7*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X44,F2,Z173)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z173)
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z173)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z173)
       deallocate(Z173)
       deallocate(X44)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S177),size(S177),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'3241',S177,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S178(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S178)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X42),size(X42),'3124',-1.000,
     & X42,S178)
       deallocate(S178)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z171(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K7*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X42,F2,Z171)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z171)
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z171)
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z171)
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z171)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z171)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z171)
       deallocate(Z171)
       deallocate(X42)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q35(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q35),size(Q35),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q35,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(Q36(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q36)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X5),size(X5),'21',1.000,X5,
     & Q36)
       deallocate(Q36)
C
       call sumx_sorted21(N0,N1,N0,N1,
     & N0,M1,M1,N1,X5,FAHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(Z5(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K7*K9*K6*K6
       I3=K5
       call EGEMM(I1,I2,I3,X5,F2,Z5)
       deallocate(F2)
C
       V3A=V3A-Z5
       call sum_stripe(6,shape(V3A),size(V3A),'123465',
     & 1.000,V3A,Z5)
       call sum_stripe(6,shape(V3A),size(V3A),'123564',
     & -1.000,V3A,Z5)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q37(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(Q37),size(Q37),shape(B1),size(B1),
     & (/N1-N1,M1-M1/),'21',Q37,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(Q38(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q38)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X6),size(X6),'21',1.000,X6,
     & Q38)
       deallocate(Q38)
C
       call sumx_sorted21(N0,N1,N0,N1,
     & M1,N1,M1,N1,X6,FAHH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(Z6(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K7*K9*K6*K6
       I3=K7
       call EGEMM(I1,I2,I3,X6,F2,Z6)
       deallocate(F2)
C
       V3A=V3A-Z6
       call sum_stripe(6,shape(V3A),size(V3A),'123465',
     & 1.000,V3A,Z6)
       call sum_stripe(6,shape(V3A),size(V3A),'123564',
     & -1.000,V3A,Z6)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q39(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q40(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q39,B2,Q40)
       deallocate(B2)
C
       call sum_stripe(2,shape(X7),size(X7),'21',-1.000,X7,
     & Q40)
       deallocate(Q40)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & M2,N3,M2,N3,X7,FAPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M2-N1,M2-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z7(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K7*K7*K9*K6
       I3=K6
       call EGEMM(I1,I2,I3,X7,F2,Z7)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234561',
     & 1.000,V3A,Z7)
       call sum_stripe(6,shape(V3A),size(V3A),'134562',
     & -1.000,V3A,Z7)
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q41(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q43(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q41,B2,Q43)
       deallocate(B2)
C
       call sum_stripe(2,shape(X9),size(X9),'21',-1.000,X9,
     & Q43)
       deallocate(Q43)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & N1,M2,N1,M2,X9,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N1-N1,M2-N1,M2-N1,M1-N0,M1-N0,M1-M1/),'312456',t3A,F2)
       allocate(Z9(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K7*K6*K6
       I3=K9
       call EGEMM(I1,I2,I3,X9,F2,Z9)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124563',
     & 1.000,V3A,Z9)
       deallocate(Z9)
       deallocate(X9)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q42(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q41,B2,Q42)
       deallocate(B2)
C
       call sum_stripe(2,shape(X8),size(X8),'21',-1.000,X8,
     & Q42)
       deallocate(Q42)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & N1,M2,M2,N3,X8,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N1-N1,M2-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'213456',t3A,F2)
       allocate(Z8(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K7*K7*K9*K6
       I3=K9
       call EGEMM(I1,I2,I3,X8,F2,Z8)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234561',
     & -1.000,V3A,Z8)
       call sum_stripe(6,shape(V3A),size(V3A),'134562',
     & 1.000,V3A,Z8)
       deallocate(Z8)
       deallocate(X8)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S181(M1+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S181)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S181),size(S181),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'2341',S181,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,M2-N1/),'3412',t2A,D2)
       allocate(S183(M2+1:N3,M2+1:N3,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K6*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S183)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',-0.500,
     & X3,S183)
       deallocate(S183)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S181),size(S181),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'3421',S181,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S184(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S184)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X32),size(X32),'2314',-1.000,
     & X32,S184)
       deallocate(S184)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S181),size(S181),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'2431',S181,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S185(M2+1:N3,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S185)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X33),size(X33),'2314',-1.000,
     & X33,S185)
       deallocate(S185)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S181),size(S181),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N0-N0,M1-M1/),'4231',S181,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S206(M1+1:N1,N0+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S206)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S206),size(S206),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'2314',S206,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S207(M2+1:N3,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S207)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',1.000,X1,
     & S207)
       deallocate(S207)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S181),size(S181),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'3241',S181,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S209(M2+1:N3,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S209)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S209),size(S209),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'2314',S209,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S210(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S210)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',-1.000,
     & X3,S210)
       deallocate(S210)
C
       call sumx_sorted2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M2,N3,M2,N3,M1,N1,X3,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(Z3(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K6
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,X3,D2,Z3)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'345126',
     & -1.000,V3A,Z3)
       call sum_stripe(6,shape(V3A),size(V3A),'346125',
     & 1.000,V3A,Z3)
       call sum_stripe(6,shape(V3A),size(V3A),'356124',
     & -1.000,V3A,Z3)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S181),size(S181),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'3241',S181,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S211(N1+1:M2,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S211)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S211),size(S211),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2314',S211,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S212(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S212)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',-1.000,
     & X4,S212)
       deallocate(S212)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S181),size(S181),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'2341',S181,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S182(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S182)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2314',-0.500,
     & X4,S182)
       deallocate(S182)
C
       call sumx_sorted2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M2,N3,N1,M2,M1,N1,X4,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,M2-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(Z4(M2+1:N3,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K6
       I2=K7*K7*K6
       I3=K3
       call EGEMM(I1,I2,I3,X4,D2,Z4)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'245136',
     & 1.000,V3A,Z4)
       call sum_stripe(6,shape(V3A),size(V3A),'145236',
     & -1.000,V3A,Z4)
       call sum_stripe(6,shape(V3A),size(V3A),'246135',
     & -1.000,V3A,Z4)
       call sum_stripe(6,shape(V3A),size(V3A),'146235',
     & 1.000,V3A,Z4)
       call sum_stripe(6,shape(V3A),size(V3A),'256134',
     & 1.000,V3A,Z4)
       call sum_stripe(6,shape(V3A),size(V3A),'156234',
     & -1.000,V3A,Z4)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S206),size(S206),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3214',S206,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S208(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S208)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S208)
       deallocate(S208)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q44(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q44),size(Q44),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q44,B1)
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S195(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S195)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S195)
       deallocate(S195)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q44),size(Q44),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q44,B1)
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,M2-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S193(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S193)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S193)
       deallocate(S193)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S190(M1+1:N1,M1+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S190)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S190),size(S190),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3412',S190,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S191(M2+1:N3,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S191)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-0.500,
     & X1,S191)
       deallocate(S191)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S190),size(S190),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'4312',S190,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S194(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S194)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',0.500,X2,
     & S194)
       deallocate(S194)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S196(M1+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S196)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S196),size(S196),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S196,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S197(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S197)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X32),size(X32),'2314',-1.000,
     & X32,S197)
       deallocate(S197)
C
       allocate(D2(N0+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z91(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K9
       I2=K7*K6*K6
       I3=K1
       call EGEMM(I1,I2,I3,X32,D2,Z91)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'125346',
     & -1.000,V3A,Z91)
       call sum_stripe(6,shape(V3A),size(V3A),'124356',
     & 1.000,V3A,Z91)
       call sum_stripe(6,shape(V3A),size(V3A),'126345',
     & 1.000,V3A,Z91)
       call sum_stripe(6,shape(V3A),size(V3A),'124365',
     & -1.000,V3A,Z91)
       call sum_stripe(6,shape(V3A),size(V3A),'126354',
     & -1.000,V3A,Z91)
       call sum_stripe(6,shape(V3A),size(V3A),'125364',
     & 1.000,V3A,Z91)
       deallocate(Z91)
       deallocate(X32)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S196),size(S196),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S196,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S198(M2+1:N3,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S198)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X33),size(X33),'2314',1.000,
     & X33,S198)
       deallocate(S198)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z92(M2+1:N3,N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1,M1+1:N1))
       I1=K7*K7*K6
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X33,D2,Z92)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'135246',
     & -1.000,V3A,Z92)
       call sum_stripe(6,shape(V3A),size(V3A),'234156',
     & -1.000,V3A,Z92)
       call sum_stripe(6,shape(V3A),size(V3A),'235146',
     & 1.000,V3A,Z92)
       call sum_stripe(6,shape(V3A),size(V3A),'134256',
     & 1.000,V3A,Z92)
       call sum_stripe(6,shape(V3A),size(V3A),'136245',
     & 1.000,V3A,Z92)
       call sum_stripe(6,shape(V3A),size(V3A),'234165',
     & 1.000,V3A,Z92)
       call sum_stripe(6,shape(V3A),size(V3A),'236145',
     & -1.000,V3A,Z92)
       call sum_stripe(6,shape(V3A),size(V3A),'134265',
     & -1.000,V3A,Z92)
       call sum_stripe(6,shape(V3A),size(V3A),'136254',
     & -1.000,V3A,Z92)
       call sum_stripe(6,shape(V3A),size(V3A),'235164',
     & -1.000,V3A,Z92)
       call sum_stripe(6,shape(V3A),size(V3A),'236154',
     & 1.000,V3A,Z92)
       call sum_stripe(6,shape(V3A),size(V3A),'135264',
     & 1.000,V3A,Z92)
       deallocate(Z92)
       deallocate(X33)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S201(M2+1:N3,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S201)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S201),size(S201),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'3412',S201,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S203(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S203)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X34),size(X34),'3124',1.000,
     & X34,S203)
       deallocate(S203)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S201),size(S201),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'3412',S201,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S202(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S202)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X35),size(X35),'2134',1.000,
     & X35,S202)
       deallocate(S202)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q45(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q45)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q45),size(Q45),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q45,B1)
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S205(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S205)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S205)
       deallocate(S205)
C
       call sumx_sorted2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,M2,M1,N1,M1,N1,X2,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z2(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K9
       I2=K7*K6*K6
       I3=K1
       call EGEMM(I1,I2,I3,X2,D2,Z2)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124356',
     & -1.000,V3A,Z2)
       call sum_stripe(6,shape(V3A),size(V3A),'125346',
     & 1.000,V3A,Z2)
       call sum_stripe(6,shape(V3A),size(V3A),'126345',
     & -1.000,V3A,Z2)
       deallocate(Z2)
       deallocate(X2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q45),size(Q45),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q45,B1)
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,M2-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S204(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S204)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S204)
       deallocate(S204)
C
       call sumx_sorted2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M2,N3,M1,N1,M1,N1,X1,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z1(M2+1:N3,N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1,M1+1:N1))
       I1=K7*K7*K6
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X1,D2,Z1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234156',
     & -1.000,V3A,Z1)
       call sum_stripe(6,shape(V3A),size(V3A),'134256',
     & 1.000,V3A,Z1)
       call sum_stripe(6,shape(V3A),size(V3A),'235146',
     & 1.000,V3A,Z1)
       call sum_stripe(6,shape(V3A),size(V3A),'135246',
     & -1.000,V3A,Z1)
       call sum_stripe(6,shape(V3A),size(V3A),'236145',
     & -1.000,V3A,Z1)
       call sum_stripe(6,shape(V3A),size(V3A),'136245',
     & 1.000,V3A,Z1)
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S186(N1+1:M2,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S186)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S186),size(S186),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'3412',S186,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S187(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S187)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X34),size(X34),'2134',-1.000,
     & X34,S187)
       deallocate(S187)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4132',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S188(M2+1:N3,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S188)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S188),size(S188),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'3412',S188,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S192(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S192)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X34),size(X34),'3124',-1.000,
     & X34,S192)
       deallocate(S192)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S188),size(S188),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'3412',S188,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S189(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S189)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X35),size(X35),'2134',-1.000,
     & X35,S189)
       deallocate(S189)
C
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(Z94(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K6
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,X35,D2,Z94)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'356124',
     & 1.000,V3A,Z94)
       call sum_stripe(6,shape(V3A),size(V3A),'356214',
     & -1.000,V3A,Z94)
       call sum_stripe(6,shape(V3A),size(V3A),'346125',
     & -1.000,V3A,Z94)
       call sum_stripe(6,shape(V3A),size(V3A),'346215',
     & 1.000,V3A,Z94)
       call sum_stripe(6,shape(V3A),size(V3A),'345126',
     & 1.000,V3A,Z94)
       call sum_stripe(6,shape(V3A),size(V3A),'345216',
     & -1.000,V3A,Z94)
       deallocate(Z94)
       deallocate(X35)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S199(N1+1:M2,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S199)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S199),size(S199),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'3412',S199,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S200(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S200)
       deallocate(D1)
       deallocate(B2)
       deallocate(S199)
C
       call sum_stripe(4,shape(X34),size(X34),'2134',-1.000,
     & X34,S200)
       deallocate(S200)
C
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,M2-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(Z93(M2+1:N3,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K6
       I2=K7*K7*K6
       I3=K3
       call EGEMM(I1,I2,I3,X34,D2,Z93)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'256134',
     & 1.000,V3A,Z93)
       call sum_stripe(6,shape(V3A),size(V3A),'156234',
     & -1.000,V3A,Z93)
       call sum_stripe(6,shape(V3A),size(V3A),'246135',
     & -1.000,V3A,Z93)
       call sum_stripe(6,shape(V3A),size(V3A),'146235',
     & 1.000,V3A,Z93)
       call sum_stripe(6,shape(V3A),size(V3A),'245136',
     & 1.000,V3A,Z93)
       call sum_stripe(6,shape(V3A),size(V3A),'145236',
     & -1.000,V3A,Z93)
       deallocate(Z93)
       deallocate(X34)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'1324',VBHPPH,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-M2,M2-M2,M1-N0,M1-M1/),'412356',t3B3,F2)
       allocate(Z27(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K7*K6*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z27)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z27)
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z27)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z27)
       deallocate(Z27)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,M1-N0/),'1324',VBHPPH,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-M2,M2-M2,M1-N0,M1-M1/),'412356',t3B3,F2)
       allocate(Z28(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K7*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z28)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z28)
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z28)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z28)
       deallocate(Z28)
C
       call sumx_sorted3(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,M1,N1,HT3A,V3A,1.0)
       deallocate(V3A)
C
       end
