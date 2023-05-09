       subroutine t3D111111_update(N0,N1,N2,N3,HT3D,shift,
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
       real*8 HT3D(N2+1:N3,N2+1:N3,N2+1:M2,N0+1:N2,N0+1:N2,M1+1:N2)
C
       real*8,allocatable::V3D(:,:,:,:,:,:)
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F1(:,:,:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S1(:,:,:,:)
       real*8,allocatable::S2(:,:,:,:)
       real*8,allocatable::S3(:,:,:,:)
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::S5(:,:,:,:)
       real*8,allocatable::S7(:,:,:,:)
       real*8,allocatable::S6(:,:,:,:)
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
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
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q14(:,:)
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
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S57(:,:,:,:)
       real*8,allocatable::S58(:,:,:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::S60(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::S62(:,:,:,:)
       real*8,allocatable::S64(:,:,:,:)
       real*8,allocatable::S66(:,:,:,:)
       real*8,allocatable::S68(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S70(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::S72(:,:,:,:)
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::S74(:,:,:,:)
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S76(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::S78(:,:,:,:)
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::S80(:,:,:,:)
       real*8,allocatable::S81(:,:,:,:)
       real*8,allocatable::S82(:,:,:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::S84(:,:,:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::S86(:,:,:,:)
       real*8,allocatable::S87(:,:,:,:)
       real*8,allocatable::S88(:,:,:,:)
       real*8,allocatable::S89(:,:,:,:)
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
       real*8,allocatable::S128(:,:,:,:)
       real*8,allocatable::S129(:,:,:,:)
       real*8,allocatable::S130(:,:,:,:)
       real*8,allocatable::S131(:,:,:,:)
       real*8,allocatable::S132(:,:,:,:)
       real*8,allocatable::S134(:,:,:,:)
       real*8,allocatable::S136(:,:,:,:)
       real*8,allocatable::S133(:,:,:,:)
       real*8,allocatable::S135(:,:,:,:)
       real*8,allocatable::S137(:,:,:,:)
       real*8,allocatable::S138(:,:,:,:)
       real*8,allocatable::S139(:,:,:,:)
       real*8,allocatable::S140(:,:,:,:)
       real*8,allocatable::S141(:,:,:,:)
       real*8,allocatable::S142(:,:,:,:)
       real*8,allocatable::S143(:,:,:,:)
       real*8,allocatable::S144(:,:,:,:)
       real*8,allocatable::S145(:,:,:,:)
       real*8,allocatable::S146(:,:,:,:)
       real*8,allocatable::S147(:,:,:,:)
       real*8,allocatable::S148(:,:,:,:)
       real*8,allocatable::S149(:,:,:,:)
       real*8,allocatable::S150(:,:,:,:)
       real*8,allocatable::S151(:,:,:,:)
       real*8,allocatable::S152(:,:,:,:)
       real*8,allocatable::S153(:,:,:,:)
       real*8,allocatable::S155(:,:,:,:)
       real*8,allocatable::S157(:,:,:,:)
       real*8,allocatable::S159(:,:,:,:)
       real*8,allocatable::S160(:,:,:,:)
       real*8,allocatable::S161(:,:,:,:)
       real*8,allocatable::S162(:,:,:,:)
       real*8,allocatable::S154(:,:,:,:)
       real*8,allocatable::S156(:,:,:,:)
       real*8,allocatable::S158(:,:,:,:)
       real*8,allocatable::S163(:,:,:,:)
       real*8,allocatable::S164(:,:,:,:)
       real*8,allocatable::S165(:,:,:,:)
       real*8,allocatable::S166(:,:,:,:)
       real*8,allocatable::S167(:,:,:,:)
       real*8,allocatable::S168(:,:,:,:)
       real*8,allocatable::S169(:,:,:,:)
       real*8,allocatable::S170(:,:,:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::S171(:,:,:,:)
       real*8,allocatable::S172(:,:,:,:)
       real*8,allocatable::S173(:,:,:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::S174(:,:,:,:)
       real*8,allocatable::S175(:,:,:,:)
       real*8,allocatable::S176(:,:,:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q36(:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::Q38(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::S178(:,:,:,:)
       real*8,allocatable::S177(:,:,:,:)
       real*8,allocatable::S179(:,:,:,:)
       real*8,allocatable::S181(:,:,:,:)
       real*8,allocatable::S182(:,:,:,:)
       real*8,allocatable::S180(:,:,:,:)
       real*8,allocatable::S183(:,:,:,:)
       real*8,allocatable::S184(:,:,:,:)
       real*8,allocatable::S185(:,:,:,:)
       real*8,allocatable::S186(:,:,:,:)
       real*8,allocatable::S187(:,:,:,:)
       real*8,allocatable::S191(:,:,:,:)
       real*8,allocatable::S192(:,:,:,:)
       real*8,allocatable::S188(:,:,:,:)
       real*8,allocatable::S189(:,:,:,:)
       real*8,allocatable::S193(:,:,:,:)
       real*8,allocatable::S190(:,:,:,:)
       real*8,allocatable::S194(:,:,:,:)
       real*8,allocatable::S202(:,:,:,:)
       real*8,allocatable::S195(:,:,:,:)
       real*8,allocatable::S200(:,:,:,:)
       real*8,allocatable::S205(:,:,:,:)
       real*8,allocatable::S201(:,:,:,:)
       real*8,allocatable::S198(:,:,:,:)
       real*8,allocatable::S199(:,:,:,:)
       real*8,allocatable::S204(:,:,:,:)
       real*8,allocatable::S196(:,:,:,:)
       real*8,allocatable::S203(:,:,:,:)
       real*8,allocatable::S197(:,:,:,:)
       real*8,allocatable::S206(:,:,:,:)
       real*8,allocatable::S207(:,:,:,:)
       real*8,allocatable::S208(:,:,:,:)
       real*8,allocatable::S209(:,:,:,:)
       real*8,allocatable::S210(:,:,:,:)
       real*8,allocatable::S211(:,:,:,:)
       real*8,allocatable::S212(:,:,:,:)
       real*8,allocatable::S220(:,:,:,:)
       real*8,allocatable::S213(:,:,:,:)
       real*8,allocatable::S216(:,:,:,:)
       real*8,allocatable::S222(:,:,:,:)
       real*8,allocatable::S217(:,:,:,:)
       real*8,allocatable::S214(:,:,:,:)
       real*8,allocatable::S221(:,:,:,:)
       real*8,allocatable::S215(:,:,:,:)
       real*8,allocatable::S218(:,:,:,:)
       real*8,allocatable::S223(:,:,:,:)
       real*8,allocatable::S219(:,:,:,:)
       real*8,allocatable::S224(:,:,:,:)
       real*8,allocatable::S225(:,:,:,:)
       real*8,allocatable::S226(:,:,:,:)
       real*8,allocatable::S227(:,:,:,:)
       real*8,allocatable::S228(:,:,:,:)
       real*8,allocatable::S229(:,:,:,:)
       real*8,allocatable::S230(:,:,:,:)
       real*8,allocatable::S231(:,:,:,:)
       real*8,allocatable::Q44(:,:)
       real*8,allocatable::Q45(:,:)
       real*8,allocatable::Q46(:,:)
       real*8,allocatable::Q47(:,:)
       real*8,allocatable::S232(:,:,:,:)
       real*8,allocatable::S233(:,:,:,:)
       real*8,allocatable::S234(:,:,:,:)
       real*8,allocatable::S235(:,:,:,:)
       real*8,allocatable::S236(:,:,:,:)
       real*8,allocatable::S237(:,:,:,:)
       real*8,allocatable::S238(:,:,:,:)
       real*8,allocatable::S239(:,:,:,:)
       real*8,allocatable::S240(:,:,:,:)
       real*8,allocatable::S241(:,:,:,:)
       real*8,allocatable::S242(:,:,:,:)
       real*8,allocatable::S243(:,:,:,:)
       real*8,allocatable::Q48(:,:)
       real*8,allocatable::Q52(:,:)
       real*8,allocatable::Q49(:,:)
       real*8,allocatable::Q50(:,:)
       real*8,allocatable::Q53(:,:)
       real*8,allocatable::Q51(:,:)
       real*8,allocatable::S244(:,:,:,:)
       real*8,allocatable::S246(:,:,:,:)
       real*8,allocatable::S245(:,:,:,:)
       real*8,allocatable::S247(:,:,:,:)
       real*8,allocatable::S248(:,:,:,:)
       real*8,allocatable::S249(:,:,:,:)
       real*8,allocatable::S250(:,:,:,:)
       real*8,allocatable::S251(:,:,:,:)
       real*8,allocatable::S252(:,:,:,:)
       real*8,allocatable::S254(:,:,:,:)
       real*8,allocatable::S255(:,:,:,:)
       real*8,allocatable::S256(:,:,:,:)
       real*8,allocatable::S257(:,:,:,:)
       real*8,allocatable::S271(:,:,:,:)
       real*8,allocatable::S272(:,:,:,:)
       real*8,allocatable::S275(:,:,:,:)
       real*8,allocatable::S276(:,:,:,:)
       real*8,allocatable::S277(:,:,:,:)
       real*8,allocatable::S278(:,:,:,:)
       real*8,allocatable::S253(:,:,:,:)
       real*8,allocatable::S273(:,:,:,:)
       real*8,allocatable::S274(:,:,:,:)
       real*8,allocatable::S279(:,:,:,:)
       real*8,allocatable::S265(:,:,:,:)
       real*8,allocatable::S268(:,:,:,:)
       real*8,allocatable::S266(:,:,:,:)
       real*8,allocatable::S261(:,:,:,:)
       real*8,allocatable::S270(:,:,:,:)
       real*8,allocatable::S262(:,:,:,:)
       real*8,allocatable::S267(:,:,:,:)
       real*8,allocatable::S258(:,:,:,:)
       real*8,allocatable::S260(:,:,:,:)
       real*8,allocatable::S263(:,:,:,:)
       real*8,allocatable::S259(:,:,:,:)
       real*8,allocatable::Q54(:,:)
       real*8,allocatable::S264(:,:,:,:)
       real*8,allocatable::S269(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::Z1(:,:,:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::Z2(:,:,:,:,:,:)
       real*8,allocatable::Z3(:,:,:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::Z4(:,:,:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::Z5(:,:,:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::Z6(:,:,:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::Z7(:,:,:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::Z8(:,:,:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::Z9(:,:,:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::Z10(:,:,:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::Z11(:,:,:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::Z12(:,:,:,:,:,:)
       real*8,allocatable::X12(:,:)
       real*8,allocatable::Z13(:,:,:,:,:,:)
       real*8,allocatable::X13(:,:)
       real*8,allocatable::Z14(:,:,:,:,:,:)
       real*8,allocatable::X14(:,:)
       real*8,allocatable::Z15(:,:,:,:,:,:)
       real*8,allocatable::X15(:,:)
       real*8,allocatable::Z16(:,:,:,:,:,:)
       real*8,allocatable::X16(:,:)
       real*8,allocatable::Z17(:,:,:,:,:,:)
       real*8,allocatable::X17(:,:)
       real*8,allocatable::Z18(:,:,:,:,:,:)
       real*8,allocatable::Z19(:,:,:,:,:,:)
       real*8,allocatable::Z20(:,:,:,:,:,:)
       real*8,allocatable::X18(:,:,:,:)
       real*8,allocatable::Z21(:,:,:,:,:,:)
       real*8,allocatable::X19(:,:,:,:)
       real*8,allocatable::Z22(:,:,:,:,:,:)
       real*8,allocatable::X20(:,:,:,:)
       real*8,allocatable::Z23(:,:,:,:,:,:)
       real*8,allocatable::Z24(:,:,:,:,:,:)
       real*8,allocatable::Z25(:,:,:,:,:,:)
       real*8,allocatable::Z26(:,:,:,:,:,:)
       real*8,allocatable::Z27(:,:,:,:,:,:)
       real*8,allocatable::Z28(:,:,:,:,:,:)
       real*8,allocatable::Z29(:,:,:,:,:,:)
       real*8,allocatable::Z30(:,:,:,:,:,:)
       real*8,allocatable::X21(:,:)
       real*8,allocatable::Z31(:,:,:,:,:,:)
       real*8,allocatable::X22(:,:)
       real*8,allocatable::Z32(:,:,:,:,:,:)
       real*8,allocatable::Z33(:,:,:,:,:,:)
       real*8,allocatable::Z34(:,:,:,:,:,:)
       real*8,allocatable::X23(:,:)
       real*8,allocatable::Z35(:,:,:,:,:,:)
       real*8,allocatable::X24(:,:)
       real*8,allocatable::Z36(:,:,:,:,:,:)
       real*8,allocatable::X25(:,:,:,:)
       real*8,allocatable::Z39(:,:,:,:,:,:)
       real*8,allocatable::X26(:,:,:,:)
       real*8,allocatable::Z38(:,:,:,:,:,:)
       real*8,allocatable::X27(:,:,:,:)
       real*8,allocatable::Z40(:,:,:,:,:,:)
       real*8,allocatable::X28(:,:,:,:)
       real*8,allocatable::Z42(:,:,:,:,:,:)
       real*8,allocatable::X29(:,:,:,:)
       real*8,allocatable::Z41(:,:,:,:,:,:)
       real*8,allocatable::X30(:,:,:,:)
       real*8,allocatable::Z43(:,:,:,:,:,:)
       real*8,allocatable::X31(:,:,:,:)
       real*8,allocatable::Z46(:,:,:,:,:,:)
       real*8,allocatable::X32(:,:,:,:)
       real*8,allocatable::Z60(:,:,:,:,:,:)
       real*8,allocatable::X33(:,:,:,:)
       real*8,allocatable::Z62(:,:,:,:,:,:)
       real*8,allocatable::X34(:,:,:,:)
       real*8,allocatable::Z56(:,:,:,:,:,:)
       real*8,allocatable::X35(:,:,:,:)
       real*8,allocatable::Z57(:,:,:,:,:,:)
       real*8,allocatable::X36(:,:,:,:)
       real*8,allocatable::Z58(:,:,:,:,:,:)
       real*8,allocatable::X37(:,:,:,:)
       real*8,allocatable::Z59(:,:,:,:,:,:)
       real*8,allocatable::X38(:,:,:,:)
       real*8,allocatable::Z61(:,:,:,:,:,:)
       real*8,allocatable::X39(:,:,:,:)
       real*8,allocatable::Z63(:,:,:,:,:,:)
       real*8,allocatable::X40(:,:)
       real*8,allocatable::Z78(:,:,:,:,:,:)
       real*8,allocatable::X41(:,:)
       real*8,allocatable::Z79(:,:,:,:,:,:)
       real*8,allocatable::Z82(:,:,:,:,:,:)
       real*8,allocatable::Z83(:,:,:,:,:,:)
       real*8,allocatable::Z84(:,:,:,:,:,:)
       real*8,allocatable::X42(:,:,:,:)
       real*8,allocatable::Z85(:,:,:,:,:,:)
       real*8,allocatable::X43(:,:,:,:)
       real*8,allocatable::Z86(:,:,:,:,:,:)
       real*8,allocatable::X44(:,:,:,:)
       real*8,allocatable::Z87(:,:,:,:,:,:)
       real*8,allocatable::X45(:,:,:,:)
       real*8,allocatable::Z88(:,:,:,:,:,:)
       real*8,allocatable::X46(:,:,:,:)
       real*8,allocatable::Z89(:,:,:,:,:,:)
       real*8,allocatable::X47(:,:,:,:)
       real*8,allocatable::Z90(:,:,:,:,:,:)
       real*8,allocatable::X48(:,:,:,:)
       real*8,allocatable::Z91(:,:,:,:,:,:)
       real*8,allocatable::X49(:,:,:,:)
       real*8,allocatable::Z92(:,:,:,:,:,:)
       real*8,allocatable::X50(:,:,:,:)
       real*8,allocatable::Z93(:,:,:,:,:,:)
       real*8,allocatable::X51(:,:,:,:)
       real*8,allocatable::Z94(:,:,:,:,:,:)
       real*8,allocatable::X52(:,:,:,:)
       real*8,allocatable::Z95(:,:,:,:,:,:)
       real*8,allocatable::X53(:,:,:,:)
       real*8,allocatable::Z96(:,:,:,:,:,:)
       real*8,allocatable::Z99(:,:,:,:,:,:)
       real*8,allocatable::Z100(:,:,:,:,:,:)
       real*8,allocatable::Z101(:,:,:,:,:,:)
       real*8,allocatable::Z102(:,:,:,:,:,:)
       real*8,allocatable::X54(:,:,:,:)
       real*8,allocatable::Z107(:,:,:,:,:,:)
       real*8,allocatable::X55(:,:,:,:)
       real*8,allocatable::Z108(:,:,:,:,:,:)
       real*8,allocatable::X56(:,:,:,:)
       real*8,allocatable::Z109(:,:,:,:,:,:)
       real*8,allocatable::Z110(:,:,:,:,:,:)
       real*8,allocatable::Z111(:,:,:,:,:,:)
       real*8,allocatable::Z112(:,:,:,:,:,:)
       real*8,allocatable::Z129(:,:,:,:,:,:)
       real*8,allocatable::Z137(:,:,:,:,:,:)
       real*8,allocatable::Z131(:,:,:,:,:,:)
       real*8,allocatable::Z139(:,:,:,:,:,:)
       real*8,allocatable::Z133(:,:,:,:,:,:)
       real*8,allocatable::Z134(:,:,:,:,:,:)
       real*8,allocatable::Z135(:,:,:,:,:,:)
       real*8,allocatable::Z136(:,:,:,:,:,:)
       real*8,allocatable::Z130(:,:,:,:,:,:)
       real*8,allocatable::Z138(:,:,:,:,:,:)
       real*8,allocatable::Z132(:,:,:,:,:,:)
       real*8,allocatable::Z140(:,:,:,:,:,:)
       real*8,allocatable::Z141(:,:,:,:,:,:)
       real*8,allocatable::Z142(:,:,:,:,:,:)
       real*8,allocatable::Z143(:,:,:,:,:,:)
       real*8,allocatable::Z144(:,:,:,:,:,:)
       real*8,allocatable::Z145(:,:,:,:,:,:)
       real*8,allocatable::Z146(:,:,:,:,:,:)
       real*8,allocatable::Z147(:,:,:,:,:,:)
       real*8,allocatable::Z148(:,:,:,:,:,:)
       real*8,allocatable::X57(:,:)
       real*8,allocatable::Z151(:,:,:,:,:,:)
       real*8,allocatable::X58(:,:)
       real*8,allocatable::Z152(:,:,:,:,:,:)
       real*8,allocatable::X59(:,:)
       real*8,allocatable::Z153(:,:,:,:,:,:)
       real*8,allocatable::X60(:,:)
       real*8,allocatable::Z154(:,:,:,:,:,:)
       real*8,allocatable::X61(:,:,:,:)
       real*8,allocatable::Z159(:,:,:,:,:,:)
       real*8,allocatable::Z160(:,:,:,:,:,:)
       real*8,allocatable::X62(:,:,:,:)
       real*8,allocatable::Z162(:,:,:,:,:,:)
       real*8,allocatable::Z164(:,:,:,:,:,:)
       real*8,allocatable::Z165(:,:,:,:,:,:)
       real*8,allocatable::X63(:,:,:,:)
       real*8,allocatable::Z167(:,:,:,:,:,:)
       real*8,allocatable::X64(:,:,:,:)
       real*8,allocatable::Z183(:,:,:,:,:,:)
       real*8,allocatable::X65(:,:,:,:)
       real*8,allocatable::Z195(:,:,:,:,:,:)
       real*8,allocatable::X66(:,:,:,:)
       real*8,allocatable::Z213(:,:,:,:,:,:)
       real*8,allocatable::X67(:,:,:,:)
       real*8,allocatable::Z219(:,:,:,:,:,:)
       real*8,allocatable::X68(:,:,:,:)
       real*8,allocatable::Z228(:,:,:,:,:,:)
       real*8,allocatable::X69(:,:,:,:)
       real*8,allocatable::Z234(:,:,:,:,:,:)
       real*8,allocatable::Z254(:,:,:,:,:,:)
       real*8,allocatable::Z255(:,:,:,:,:,:)
       real*8,allocatable::Z256(:,:,:,:,:,:)
       real*8,allocatable::Z257(:,:,:,:,:,:)
       real*8,allocatable::Z260(:,:,:,:,:,:)
       real*8,allocatable::Z261(:,:,:,:,:,:)
       real*8,allocatable::Z262(:,:,:,:,:,:)
       real*8,allocatable::Z265(:,:,:,:,:,:)
       real*8,allocatable::Z266(:,:,:,:,:,:)
       real*8,allocatable::Z267(:,:,:,:,:,:)
       real*8,allocatable::Z327(:,:,:,:,:,:)
       real*8,allocatable::Z328(:,:,:,:,:,:)
       real*8,allocatable::Z329(:,:,:,:,:,:)
       real*8,allocatable::Z345(:,:,:,:,:,:)
       real*8,allocatable::Z346(:,:,:,:,:,:)
C
       allocate(V3D(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       V3D=0.0d0
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q1(N0+1:M1,M1+1:N2))
       I1=K8*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X21(N0+1:M1,M1+1:N2))
       X21=0.0d0
       X21=X21+Q1
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q2(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X22(M1+1:N2,M1+1:N2))
       X22=0.0d0
       X22=X22+Q2
       deallocate(Q2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q3(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z33(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,Q3,F2,Z33)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234561',
     & 1.000,V3D,Z33)
       call sum_stripe(6,shape(V3D),size(V3D),'134562',
     & -1.000,V3D,Z33)
       deallocate(Z33)
       deallocate(Q3)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q4(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z34(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K0
       call EGEMM(I1,I2,I3,Q4,F2,Z34)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234561',
     & 1.000,V3D,Z34)
       call sum_stripe(6,shape(V3D),size(V3D),'134562',
     & -1.000,V3D,Z34)
       deallocate(Z34)
       deallocate(Q4)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q5(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X23(M2+1:N3,N2+1:M2))
       X23=0.0d0
       X23=X23+Q5
       deallocate(Q5)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q6(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X24(N2+1:M2,N2+1:M2))
       X24=0.0d0
       X24=X24+Q6
       deallocate(Q6)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHH),size(VCHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M1-N0,M1-N0/),'3412',VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S1(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X1=0.0d0
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S1)
C
       allocate(X25(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X25=0.0d0
       call sum_stripe(4,shape(X25),size(X25),'2134',1.000,
     & X25,S1)
       deallocate(S1)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHH),size(VCHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M1-N0,M1-N0/),'3412',VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S2(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X26(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X26=0.0d0
       call sum_stripe(4,shape(X26),size(X26),'2134',1.000,
     & X26,S2)
       deallocate(S2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,M1-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S3(M1+1:N2,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X27(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X27=0.0d0
       call sum_stripe(4,shape(X27),size(X27),'3124',1.000,
     & X27,S3)
C
       allocate(X28(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X28=0.0d0
       call sum_stripe(4,shape(X28),size(X28),'3124',1.000,
     & X28,S3)
       deallocate(S3)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,M1-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S4(M1+1:N2,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X29(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X29=0.0d0
       call sum_stripe(4,shape(X29),size(X29),'3124',1.000,
     & X29,S4)
       deallocate(S4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S5(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       X3=0.0d0
       call sum_stripe(4,shape(X3),size(X3),'2134',1.000,X3,
     & S5)
C
       allocate(X30(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       X30=0.0d0
       call sum_stripe(4,shape(X30),size(X30),'3124',1.000,
     & X30,S5)
       deallocate(S5)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S7(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'3124',-1.000,
     & X3,S7)
C
       call sum_stripe(4,shape(X30),size(X30),'2134',-1.000,
     & X30,S7)
       deallocate(S7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S6(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X31(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       X31=0.0d0
       call sum_stripe(4,shape(X31),size(X31),'2134',1.000,
     & X31,S6)
C
       call sum_stripe(4,shape(X31),size(X31),'3124',-1.000,
     & X31,S6)
       deallocate(S6)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCAPPP),size(VCAPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N2-N2,N2-N2/),'1234',VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S8(M1+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K4
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X30),size(X30),'4123',1.000,
     & X30,S8)
       deallocate(S8)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCAPPP),size(VCAPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N2-N2,N2-N2/),'1234',VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S9(M1+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K4
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X31),size(X31),'4123',-1.000,
     & X31,S9)
       deallocate(S9)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCAPPP),size(VCAPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N2-N2,N2-N2/),'1234',VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S10(M1+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K4
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'4123',-1.000,
     & X3,S10)
       deallocate(S10)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S11(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X4=0.0d0
       call sum_stripe(4,shape(X4),size(X4),'3124',-1.000,
     & X4,S11)
C
       allocate(X32(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X32=0.0d0
       call sum_stripe(4,shape(X32),size(X32),'3124',1.000,
     & X32,S11)
       deallocate(S11)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S13(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       X6=0.0d0
       call sum_stripe(4,shape(X6),size(X6),'3124',-1.000,
     & X6,S13)
C
       allocate(X33(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       X33=0.0d0
       call sum_stripe(4,shape(X33),size(X33),'3124',1.000,
     & X33,S13)
       deallocate(S13)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S15(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X34(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X34=0.0d0
       call sum_stripe(4,shape(X34),size(X34),'3124',1.000,
     & X34,S15)
       deallocate(S15)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S16(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X35(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X35=0.0d0
       call sum_stripe(4,shape(X35),size(X35),'3124',1.000,
     & X35,S16)
       deallocate(S16)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S17(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X36(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       X36=0.0d0
       call sum_stripe(4,shape(X36),size(X36),'3124',1.000,
     & X36,S17)
       deallocate(S17)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S18(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X37(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X37=0.0d0
       call sum_stripe(4,shape(X37),size(X37),'3124',1.000,
     & X37,S18)
       deallocate(S18)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S12(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X5=0.0d0
       call sum_stripe(4,shape(X5),size(X5),'3124',-1.000,
     & X5,S12)
C
       allocate(X38(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X38=0.0d0
       call sum_stripe(4,shape(X38),size(X38),'3124',1.000,
     & X38,S12)
       deallocate(S12)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S14(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X7=0.0d0
       call sum_stripe(4,shape(X7),size(X7),'3124',-1.000,
     & X7,S14)
C
       allocate(X39(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X39=0.0d0
       call sum_stripe(4,shape(X39),size(X39),'3124',1.000,
     & X39,S14)
       deallocate(S14)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S19(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'4123',1.000,X4,
     & S19)
C
       call sum_stripe(4,shape(X32),size(X32),'4123',-1.000,
     & X32,S19)
       deallocate(S19)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S21(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'4123',1.000,X6,
     & S21)
C
       call sum_stripe(4,shape(X33),size(X33),'4123',-1.000,
     & X33,S21)
       deallocate(S21)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S23(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X34),size(X34),'4123',-1.000,
     & X34,S23)
       deallocate(S23)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S24(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X35),size(X35),'4123',-1.000,
     & X35,S24)
       deallocate(S24)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S25(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X36),size(X36),'4123',-1.000,
     & X36,S25)
       deallocate(S25)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S26(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X37),size(X37),'4123',-1.000,
     & X37,S26)
       deallocate(S26)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S20(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'4123',1.000,X5,
     & S20)
C
       call sum_stripe(4,shape(X38),size(X38),'4123',-1.000,
     & X38,S20)
       deallocate(S20)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S22(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'4123',1.000,X7,
     & S22)
C
       call sum_stripe(4,shape(X39),size(X39),'4123',-1.000,
     & X39,S22)
       deallocate(S22)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q7(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q7)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X12(N0+1:M1,M1+1:N2))
       X12=0.0d0
       call sum_stripe(2,shape(X12),size(X12),'21',1.000,
     & X12,Q7)
       deallocate(Q7)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,M1-N0/),'12',FBHP,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q8(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q8)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X13(M1+1:N2,M1+1:N2))
       X13=0.0d0
       call sum_stripe(2,shape(X13),size(X13),'21',1.000,
     & X13,Q8)
       deallocate(Q8)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,M2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q9(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q9)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X40(M2+1:N3,N2+1:M2))
       X40=0.0d0
       call sum_stripe(2,shape(X40),size(X40),'21',1.000,
     & X40,Q9)
       deallocate(Q9)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,N2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q10(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q10)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X41(N2+1:M2,N2+1:M2))
       X41=0.0d0
       call sum_stripe(2,shape(X41),size(X41),'21',1.000,
     & X41,Q10)
       deallocate(Q10)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,M2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q11(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q11)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X16(M2+1:N3,N2+1:M2))
       X16=0.0d0
       call sum_stripe(2,shape(X16),size(X16),'21',-1.000,
     & X16,Q11)
       deallocate(Q11)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,N2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q12(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q12)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X17(N2+1:M2,N2+1:M2))
       X17=0.0d0
       call sum_stripe(2,shape(X17),size(X17),'21',-1.000,
     & X17,Q12)
       deallocate(Q12)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M1-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S27(M1+1:N2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S27),size(S27),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'2314',S27,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(Z82(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K0*K0*K0
       I3=K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z82)
       deallocate(D1)
       deallocate(F2)
C
       V3D=V3D+0.500*Z82
       call sum_stripe(6,shape(V3D),size(V3D),'123546',
     & -0.500,V3D,Z82)
       call sum_stripe(6,shape(V3D),size(V3D),'123465',
     & -0.500,V3D,Z82)
       call sum_stripe(6,shape(V3D),size(V3D),'123564',
     & 0.500,V3D,Z82)
       call sum_stripe(6,shape(V3D),size(V3D),'123645',
     & 0.500,V3D,Z82)
       call sum_stripe(6,shape(V3D),size(V3D),'123654',
     & -0.500,V3D,Z82)
       deallocate(Z82)
       deallocate(S27)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M1-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S28(M1+1:N2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S28),size(S28),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M1-M1,M1-M1/),'2314',S28,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(Z83(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K0*K0*K0
       I3=K8*K5
       call EGEMM(I1,I2,I3,D1,F2,Z83)
       deallocate(D1)
       deallocate(F2)
C
       V3D=V3D+Z83
       call sum_stripe(6,shape(V3D),size(V3D),'123546',
     & -1.000,V3D,Z83)
       call sum_stripe(6,shape(V3D),size(V3D),'123465',
     & -1.000,V3D,Z83)
       call sum_stripe(6,shape(V3D),size(V3D),'123564',
     & 1.000,V3D,Z83)
       call sum_stripe(6,shape(V3D),size(V3D),'123645',
     & 1.000,V3D,Z83)
       call sum_stripe(6,shape(V3D),size(V3D),'123654',
     & -1.000,V3D,Z83)
       deallocate(Z83)
       deallocate(S28)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,M1-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S29(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S29),size(S29),shape(D1),size(D1),
     & (/M1-M1,M1-M1,M1-M1,M1-M1/),'2314',S29,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(Z84(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K0*K0*K0
       I3=K8*K8
       call EGEMM(I1,I2,I3,D1,F2,Z84)
       deallocate(D1)
       deallocate(F2)
C
       V3D=V3D+0.500*Z84
       call sum_stripe(6,shape(V3D),size(V3D),'123546',
     & -0.500,V3D,Z84)
       call sum_stripe(6,shape(V3D),size(V3D),'123465',
     & -0.500,V3D,Z84)
       call sum_stripe(6,shape(V3D),size(V3D),'123564',
     & 0.500,V3D,Z84)
       call sum_stripe(6,shape(V3D),size(V3D),'123645',
     & 0.500,V3D,Z84)
       call sum_stripe(6,shape(V3D),size(V3D),'123654',
     & -0.500,V3D,Z84)
       deallocate(Z84)
       deallocate(S29)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S30(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X42(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X42=0.0d0
       call sum_stripe(4,shape(X42),size(X42),'3124',1.000,
     & X42,S30)
       deallocate(S30)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S31(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X43(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X43=0.0d0
       call sum_stripe(4,shape(X43),size(X43),'3124',1.000,
     & X43,S31)
       deallocate(S31)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S32(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X44(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       X44=0.0d0
       call sum_stripe(4,shape(X44),size(X44),'3124',1.000,
     & X44,S32)
       deallocate(S32)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S33(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X45(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X45=0.0d0
       call sum_stripe(4,shape(X45),size(X45),'3124',1.000,
     & X45,S33)
       deallocate(S33)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S34(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X46(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X46=0.0d0
       call sum_stripe(4,shape(X46),size(X46),'3124',1.000,
     & X46,S34)
       deallocate(S34)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S35(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X47(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X47=0.0d0
       call sum_stripe(4,shape(X47),size(X47),'3124',1.000,
     & X47,S35)
       deallocate(S35)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S36(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X48(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       X48=0.0d0
       call sum_stripe(4,shape(X48),size(X48),'3124',1.000,
     & X48,S36)
       deallocate(S36)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S37(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X49(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X49=0.0d0
       call sum_stripe(4,shape(X49),size(X49),'3124',1.000,
     & X49,S37)
       deallocate(S37)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M1-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S38(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X50(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X50=0.0d0
       call sum_stripe(4,shape(X50),size(X50),'3124',1.000,
     & X50,S38)
       deallocate(S38)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S39(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X51(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X51=0.0d0
       call sum_stripe(4,shape(X51),size(X51),'3124',1.000,
     & X51,S39)
       deallocate(S39)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S40(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X52(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       X52=0.0d0
       call sum_stripe(4,shape(X52),size(X52),'3124',1.000,
     & X52,S40)
       deallocate(S40)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S41(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X53(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X53=0.0d0
       call sum_stripe(4,shape(X53),size(X53),'3124',1.000,
     & X53,S41)
       deallocate(S41)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q13(N0+1:M1,M1+1:N2))
       I1=K8*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
C
       X12=X12+Q13
       deallocate(Q13)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M1-N0/),'3142',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q14(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       X13=X13+Q14
       deallocate(Q14)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S42(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S42),size(S42),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,M1-M1/),'2341',S42,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z99(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z99)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z99)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z99)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z99)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z99)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z99)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z99)
       deallocate(Z99)
       deallocate(S42)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S43(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S43),size(S43),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,M1-M1/),'2341',S43,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z100(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z100)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z100)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z100)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z100)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z100)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z100)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z100)
       deallocate(Z100)
       deallocate(S43)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S44(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S44),size(S44),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2341',S44,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z101(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z101)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z101)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z101)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z101)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z101)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z101)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z101)
       deallocate(Z101)
       deallocate(S44)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S45(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S45),size(S45),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N2-N2,M1-M1/),'2341',S45,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z102(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z102)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z102)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z102)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z102)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z102)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z102)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z102)
       deallocate(Z102)
       deallocate(S45)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S46(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X46),size(X46),'4123',1.000,
     & X46,S46)
       deallocate(S46)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S47(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X47),size(X47),'4123',1.000,
     & X47,S47)
       deallocate(S47)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S48(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X48),size(X48),'4123',1.000,
     & X48,S48)
       deallocate(S48)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S49(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X49),size(X49),'4123',1.000,
     & X49,S49)
       deallocate(S49)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S50(N2+1:M2,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X54(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       X54=0.0d0
       call sum_stripe(4,shape(X54),size(X54),'4123',1.000,
     & X54,S50)
       deallocate(S50)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S51(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X55(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       X55=0.0d0
       call sum_stripe(4,shape(X55),size(X55),'4123',1.000,
     & X55,S51)
       deallocate(S51)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S52(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X56(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       X56=0.0d0
       call sum_stripe(4,shape(X56),size(X56),'4123',1.000,
     & X56,S52)
       deallocate(S52)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S53(N2+1:M2,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S53),size(S53),shape(D1),size(D1),
     & (/M2-M2,M2-M2,N2-N2,N2-N2/),'2341',S53,D1)
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,M2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z110(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,D1,F2,Z110)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & 0.500,V3D,Z110)
       call sum_stripe(6,shape(V3D),size(V3D),'245631',
     & -0.500,V3D,Z110)
       deallocate(Z110)
       deallocate(S53)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S54(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S54),size(S54),shape(D1),size(D1),
     & (/M2-M2,N2-N2,N2-N2,N2-N2/),'2341',S54,D1)
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z111(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K0*K6
       call EGEMM(I1,I2,I3,D1,F2,Z111)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & 1.000,V3D,Z111)
       call sum_stripe(6,shape(V3D),size(V3D),'245631',
     & -1.000,V3D,Z111)
       deallocate(Z111)
       deallocate(S54)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S55(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S55),size(S55),shape(D1),size(D1),
     & (/N2-N2,N2-N2,N2-N2,N2-N2/),'2341',S55,D1)
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z112(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K0*K0
       call EGEMM(I1,I2,I3,D1,F2,Z112)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & 0.500,V3D,Z112)
       call sum_stripe(6,shape(V3D),size(V3D),'245631',
     & -0.500,V3D,Z112)
       deallocate(Z112)
       deallocate(S55)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q15(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       X40=X40+Q15
       deallocate(Q15)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q16(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q16)
       deallocate(D1)
       deallocate(B2)
C
       X41=X41+Q16
       deallocate(Q16)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S56(N2+1:M2,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X54),size(X54),'3124',-1.000,
     & X54,S56)
       deallocate(S56)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S57(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X55),size(X55),'3124',-1.000,
     & X55,S57)
       deallocate(S57)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S58(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X56),size(X56),'3124',-1.000,
     & X56,S58)
       deallocate(S58)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q17(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q17)
       deallocate(D1)
       deallocate(B2)
C
       X16=X16-Q17
       deallocate(Q17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q18(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q18)
       deallocate(D1)
       deallocate(B2)
C
       X17=X17-Q18
       deallocate(Q18)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S59(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S59)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X27),size(X27),'2314',1.000,
     & X27,S59)
C
       call sum_stripe(4,shape(X28),size(X28),'2314',1.000,
     & X28,S59)
       deallocate(S59)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S60(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S60)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X29),size(X29),'2314',1.000,
     & X29,S60)
       deallocate(S60)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S61(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S61)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2413',-1.000,
     & X3,S61)
C
       call sum_stripe(4,shape(X30),size(X30),'3412',-1.000,
     & X30,S61)
       deallocate(S61)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S63(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S63)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'3412',1.000,X3,
     & S63)
C
       call sum_stripe(4,shape(X30),size(X30),'2413',1.000,
     & X30,S63)
       deallocate(S63)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S62(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S62)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X31),size(X31),'2413',-1.000,
     & X31,S62)
C
       call sum_stripe(4,shape(X31),size(X31),'3412',1.000,
     & X31,S62)
       deallocate(S62)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S64(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S64)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S64),size(S64),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,M1-M1/),'3412',S64,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z129(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z129)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z129)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z129)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z129)
       deallocate(Z129)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S64),size(S64),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,M1-M1/),'3412',S64,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z137(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z137)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z137)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z137)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z137)
       deallocate(Z137)
       deallocate(S64)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S66(N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S66)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S66),size(S66),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,M1-M1/),'3412',S66,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z131(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z131)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z131)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z131)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z131)
       deallocate(Z131)
C
       allocate(D1(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S66),size(S66),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,M1-M1/),'3412',S66,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z139(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z139)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z139)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z139)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z139)
       deallocate(Z139)
       deallocate(S66)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S68(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S68)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S68),size(S68),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,M1-M1/),'3412',S68,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z133(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z133)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z133)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z133)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z133)
       deallocate(Z133)
       deallocate(S68)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S69(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S69)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S69),size(S69),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,M1-M1/),'3412',S69,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z134(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z134)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z134)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z134)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z134)
       deallocate(Z134)
       deallocate(S69)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S70(N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S70)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S70),size(S70),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,M1-M1/),'3412',S70,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z135(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z135)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z135)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z135)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z135)
       deallocate(Z135)
       deallocate(S70)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S71(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S71)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S71),size(S71),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N2-N2,M1-M1/),'3412',S71,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z136(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z136)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z136)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z136)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z136)
       deallocate(Z136)
       deallocate(S71)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S65(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S65)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S65),size(S65),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,M1-M1/),'3412',S65,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z130(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z130)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z130)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z130)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z130)
       deallocate(Z130)
C
       allocate(D1(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S65),size(S65),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,M1-M1/),'3412',S65,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z138(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z138)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z138)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z138)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z138)
       deallocate(Z138)
       deallocate(S65)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S67(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S67)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S67),size(S67),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N2-N2,M1-M1/),'3412',S67,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z132(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z132)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z132)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z132)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z132)
       deallocate(Z132)
C
       allocate(D1(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S67),size(S67),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N2-N2,M1-M1/),'3412',S67,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z140(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z140)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z140)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z140)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z140)
       deallocate(Z140)
       deallocate(S67)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S72(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S72)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S72),size(S72),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,M1-M1/),'3412',S72,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z141(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z141)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z141)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z141)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z141)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z141)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z141)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z141)
       deallocate(Z141)
       deallocate(S72)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S73(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S73)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S73),size(S73),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,M1-M1/),'3412',S73,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z142(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z142)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z142)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z142)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z142)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z142)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z142)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z142)
       deallocate(Z142)
       deallocate(S73)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S74(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S74)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S74),size(S74),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S74,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z143(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z143)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z143)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z143)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z143)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z143)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z143)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z143)
       deallocate(Z143)
       deallocate(S74)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S75(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S75)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S75),size(S75),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N2-N2,M1-M1/),'3412',S75,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z144(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z144)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z144)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z144)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z144)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z144)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z144)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z144)
       deallocate(Z144)
       deallocate(S75)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S76(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S76)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S76),size(S76),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,M1-M1/),'3412',S76,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z145(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z145)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z145)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z145)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z145)
       deallocate(Z145)
       deallocate(S76)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S77(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S77)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S77),size(S77),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,M1-M1/),'3412',S77,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z146(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z146)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z146)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z146)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z146)
       deallocate(Z146)
       deallocate(S77)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S78(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S78)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S78),size(S78),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S78,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z147(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z147)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z147)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z147)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z147)
       deallocate(Z147)
       deallocate(S78)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S79(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S79)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S79),size(S79),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N2-N2,M1-M1/),'3412',S79,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z148(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z148)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z148)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z148)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z148)
       deallocate(Z148)
       deallocate(S79)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(Q19(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q19)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X21),size(X21),'21',1.000,
     & X21,Q19)
       deallocate(Q19)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(Q20(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q20)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X22),size(X22),'21',1.000,
     & X22,Q20)
       deallocate(Q20)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',t2B,D2)
       allocate(Q21(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q21)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X57(M2+1:N3,N2+1:M2))
       X57=0.0d0
       call sum_stripe(2,shape(X57),size(X57),'21',1.000,
     & X57,Q21)
       deallocate(Q21)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',t2B,D2)
       allocate(Q22(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q22)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X58(N2+1:M2,N2+1:M2))
       X58=0.0d0
       call sum_stripe(2,shape(X58),size(X58),'21',1.000,
     & X58,Q22)
       deallocate(Q22)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',t2B,D2)
       allocate(Q23(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q23)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X59(M2+1:N3,N2+1:M2))
       X59=0.0d0
       call sum_stripe(2,shape(X59),size(X59),'21',1.000,
     & X59,Q23)
       deallocate(Q23)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',t2B,D2)
       allocate(Q24(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q24)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X60(N2+1:M2,N2+1:M2))
       X60=0.0d0
       call sum_stripe(2,shape(X60),size(X60),'21',1.000,
     & X60,Q24)
       deallocate(Q24)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(S80(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S80)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S80)
       deallocate(S80)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(S81(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S81)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X26),size(X26),'2341',1.000,
     & X26,S81)
       deallocate(S81)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(S82(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S82)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X25),size(X25),'2341',1.000,
     & X25,S82)
       deallocate(S82)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S83(N2+1:M2,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S83)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',0.500,X3,
     & S83)
       deallocate(S83)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S84(N2+1:M2,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S84)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X61(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       X61=0.0d0
       call sum_stripe(4,shape(X61),size(X61),'2314',1.000,
     & X61,S84)
       deallocate(S84)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'4132',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S85(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S85)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S85),size(S85),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M1-M1,M1-M1/),'3124',S85,D1)
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z160(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,D2,Z160)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'125346',
     & -1.000,V3D,Z160)
       call sum_stripe(6,shape(V3D),size(V3D),'135246',
     & 1.000,V3D,Z160)
       call sum_stripe(6,shape(V3D),size(V3D),'134256',
     & -1.000,V3D,Z160)
       call sum_stripe(6,shape(V3D),size(V3D),'124356',
     & 1.000,V3D,Z160)
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & 1.000,V3D,Z160)
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & -1.000,V3D,Z160)
       call sum_stripe(6,shape(V3D),size(V3D),'134265',
     & 1.000,V3D,Z160)
       call sum_stripe(6,shape(V3D),size(V3D),'124365',
     & -1.000,V3D,Z160)
       call sum_stripe(6,shape(V3D),size(V3D),'126354',
     & -1.000,V3D,Z160)
       call sum_stripe(6,shape(V3D),size(V3D),'136254',
     & 1.000,V3D,Z160)
       call sum_stripe(6,shape(V3D),size(V3D),'135264',
     & -1.000,V3D,Z160)
       call sum_stripe(6,shape(V3D),size(V3D),'125364',
     & 1.000,V3D,Z160)
       deallocate(Z160)
       deallocate(S85)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S86(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S86)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X27),size(X27),'2314',1.000,
     & X27,S86)
       deallocate(S86)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4213',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S87(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S87)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X62(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       X62=0.0d0
       call sum_stripe(4,shape(X62),size(X62),'3412',1.000,
     & X62,S87)
       deallocate(S87)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N2-N2/),'1243',VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(S88(M1+1:N2,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S88)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'3412',0.500,X1,
     & S88)
       deallocate(S88)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4213',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S89(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S89)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S89),size(S89),shape(D1),size(D1),
     & (/N2-N2,N2-N2,N2-N2,M1-M1/),'3412',S89,D1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z164(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,D1,D2,Z164)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'156234',
     & -1.000,V3D,Z164)
       call sum_stripe(6,shape(V3D),size(V3D),'156324',
     & 1.000,V3D,Z164)
       call sum_stripe(6,shape(V3D),size(V3D),'146235',
     & 1.000,V3D,Z164)
       call sum_stripe(6,shape(V3D),size(V3D),'146325',
     & -1.000,V3D,Z164)
       call sum_stripe(6,shape(V3D),size(V3D),'145236',
     & -1.000,V3D,Z164)
       call sum_stripe(6,shape(V3D),size(V3D),'145326',
     & 1.000,V3D,Z164)
       deallocate(Z164)
       deallocate(S89)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S90(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S90)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S90),size(S90),shape(D1),size(D1),
     & (/N2-N2,N2-N2,N2-N2,M1-M1/),'3142',S90,D1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z165(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,D1,D2,Z165)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'356124',
     & -1.000,V3D,Z165)
       call sum_stripe(6,shape(V3D),size(V3D),'256134',
     & 1.000,V3D,Z165)
       call sum_stripe(6,shape(V3D),size(V3D),'346125',
     & 1.000,V3D,Z165)
       call sum_stripe(6,shape(V3D),size(V3D),'246135',
     & -1.000,V3D,Z165)
       call sum_stripe(6,shape(V3D),size(V3D),'345126',
     & -1.000,V3D,Z165)
       call sum_stripe(6,shape(V3D),size(V3D),'245136',
     & 1.000,V3D,Z165)
       deallocate(Z165)
       deallocate(S90)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N2-N2/),'1243',VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(S91(M1+1:N2,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S91)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X2(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X2=0.0d0
       call sum_stripe(4,shape(X2),size(X2),'3412',0.500,X2,
     & S91)
       deallocate(S91)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S92(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S92)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X63(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       X63=0.0d0
       call sum_stripe(4,shape(X63),size(X63),'2341',1.000,
     & X63,S92)
       deallocate(S92)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S93(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S93)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X63),size(X63),'2341',1.000,
     & X63,S93)
       deallocate(S93)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S94(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S94)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X63),size(X63),'2341',1.000,
     & X63,S94)
       deallocate(S94)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S95(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S95)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X63),size(X63),'2341',1.000,
     & X63,S95)
       deallocate(S95)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S96(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S96)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X63),size(X63),'2341',1.000,
     & X63,S96)
       deallocate(S96)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N1-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S97(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S97)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X63),size(X63),'2341',1.000,
     & X63,S97)
       deallocate(S97)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S98(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S98)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X63),size(X63),'2341',1.000,
     & X63,S98)
       deallocate(S98)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S99(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S99)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X63),size(X63),'2341',1.000,
     & X63,S99)
       deallocate(S99)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S100(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S100)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X62),size(X62),'2341',-1.000,
     & X62,S100)
       deallocate(S100)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S101(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S101)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X62),size(X62),'2341',-1.000,
     & X62,S101)
       deallocate(S101)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S102(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S102)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X62),size(X62),'2341',-1.000,
     & X62,S102)
       deallocate(S102)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S103(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S103)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X62),size(X62),'2341',-1.000,
     & X62,S103)
       deallocate(S103)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S104(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S104)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X62),size(X62),'2341',-1.000,
     & X62,S104)
       deallocate(S104)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N1-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S105(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S105)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X62),size(X62),'2341',-1.000,
     & X62,S105)
       deallocate(S105)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S106(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S106)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X62),size(X62),'2341',-1.000,
     & X62,S106)
       deallocate(S106)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S107(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S107)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X62),size(X62),'2341',-1.000,
     & X62,S107)
       deallocate(S107)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z162(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X62,D2,Z162)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'256134',
     & 1.000,V3D,Z162)
       call sum_stripe(6,shape(V3D),size(V3D),'356124',
     & -1.000,V3D,Z162)
       call sum_stripe(6,shape(V3D),size(V3D),'246135',
     & -1.000,V3D,Z162)
       call sum_stripe(6,shape(V3D),size(V3D),'346125',
     & 1.000,V3D,Z162)
       call sum_stripe(6,shape(V3D),size(V3D),'345126',
     & -1.000,V3D,Z162)
       call sum_stripe(6,shape(V3D),size(V3D),'245136',
     & 1.000,V3D,Z162)
       deallocate(Z162)
       deallocate(X62)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S108(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S108)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X64(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X64=0.0d0
       call sum_stripe(4,shape(X64),size(X64),'2341',1.000,
     & X64,S108)
       deallocate(S108)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S109(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S109)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X64),size(X64),'2341',1.000,
     & X64,S109)
       deallocate(S109)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S110(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S110)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X64),size(X64),'2341',1.000,
     & X64,S110)
       deallocate(S110)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S111(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S111)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X64),size(X64),'2341',1.000,
     & X64,S111)
       deallocate(S111)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S112(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S112)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X64),size(X64),'2341',1.000,
     & X64,S112)
       deallocate(S112)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S113(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S113)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X64),size(X64),'2341',1.000,
     & X64,S113)
       deallocate(S113)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S114(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S114)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X64),size(X64),'2341',1.000,
     & X64,S114)
       deallocate(S114)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S115(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S115)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X64),size(X64),'2341',1.000,
     & X64,S115)
       deallocate(S115)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z183(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X64,D2,Z183)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & 1.000,V3D,Z183)
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & -1.000,V3D,Z183)
       call sum_stripe(6,shape(V3D),size(V3D),'125346',
     & -1.000,V3D,Z183)
       call sum_stripe(6,shape(V3D),size(V3D),'135246',
     & 1.000,V3D,Z183)
       call sum_stripe(6,shape(V3D),size(V3D),'124356',
     & 1.000,V3D,Z183)
       call sum_stripe(6,shape(V3D),size(V3D),'134256',
     & -1.000,V3D,Z183)
       deallocate(Z183)
       deallocate(X64)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S116(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S116)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'3412',1.000,X4,
     & S116)
       deallocate(S116)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S117(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S117)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'3412',1.000,X5,
     & S117)
       deallocate(S117)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S118(N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S118)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'3412',1.000,X6,
     & S118)
       deallocate(S118)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S119(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S119)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'3412',1.000,X7,
     & S119)
       deallocate(S119)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S120(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S120)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X65(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X65=0.0d0
       call sum_stripe(4,shape(X65),size(X65),'2341',1.000,
     & X65,S120)
       deallocate(S120)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S121(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S121)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',1.000,
     & X65,S121)
       deallocate(S121)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S122(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S122)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',1.000,
     & X65,S122)
       deallocate(S122)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S123(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S123)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',1.000,
     & X65,S123)
       deallocate(S123)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S124(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S124)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',1.000,
     & X65,S124)
       deallocate(S124)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S125(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S125)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',1.000,
     & X65,S125)
       deallocate(S125)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S126(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S126)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',1.000,
     & X65,S126)
       deallocate(S126)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,N2-N2,M1-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S127(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S127)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',1.000,
     & X65,S127)
       deallocate(S127)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S128(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S128)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X8(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X8=0.0d0
       call sum_stripe(4,shape(X8),size(X8),'3412',1.000,X8,
     & S128)
       deallocate(S128)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,M1,N2,X8,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z9(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X8,F2,Z9)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z9)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z9)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z9)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z9)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z9)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z9)
       deallocate(Z9)
       deallocate(X8)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S129(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S129)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X9(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X9=0.0d0
       call sum_stripe(4,shape(X9),size(X9),'3412',1.000,X9,
     & S129)
       deallocate(S129)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,M1,N2,X9,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z10(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X9,F2,Z10)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z10)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z10)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z10)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z10)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z10)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z10)
       deallocate(Z10)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S130(N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S130)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X10(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       X10=0.0d0
       call sum_stripe(4,shape(X10),size(X10),'3412',1.000,
     & X10,S130)
       deallocate(S130)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,N2,M2,M1,N2,X10,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z11(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,X10,F2,Z11)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z11)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z11)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z11)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z11)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z11)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z11)
       deallocate(Z11)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S131(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S131)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X11(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X11=0.0d0
       call sum_stripe(4,shape(X11),size(X11),'3412',1.000,
     & X11,S131)
       deallocate(S131)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,M1,N2,X11,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z12(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X11,F2,Z12)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z12)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z12)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z12)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z12)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z12)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z12)
       deallocate(Z12)
       deallocate(X11)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S132(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S132)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X63),size(X63),'2341',-0.500,
     & X63,S132)
C
       allocate(X66(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       X66=0.0d0
       call sum_stripe(4,shape(X66),size(X66),'2341',1.000,
     & X66,S132)
       deallocate(S132)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S134(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S134)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X63),size(X63),'2341',-0.500,
     & X63,S134)
C
       call sum_stripe(4,shape(X66),size(X66),'2341',1.000,
     & X66,S134)
       deallocate(S134)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S136(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S136)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X63),size(X63),'2341',-1.000,
     & X63,S136)
C
       call sum_stripe(4,shape(X66),size(X66),'2341',2.000,
     & X66,S136)
       deallocate(S136)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S133(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S133)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X63),size(X63),'2341',-1.000,
     & X63,S133)
C
       call sum_stripe(4,shape(X66),size(X66),'2341',2.000,
     & X66,S133)
       deallocate(S133)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S135(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S135)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X63),size(X63),'2341',-0.500,
     & X63,S135)
C
       call sum_stripe(4,shape(X66),size(X66),'2341',1.000,
     & X66,S135)
       deallocate(S135)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S137(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S137)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X63),size(X63),'2341',-0.500,
     & X63,S137)
C
       call sum_stripe(4,shape(X66),size(X66),'2341',1.000,
     & X66,S137)
       deallocate(S137)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S138(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S138)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X67(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       X67=0.0d0
       call sum_stripe(4,shape(X67),size(X67),'2341',1.000,
     & X67,S138)
       deallocate(S138)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S139(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S139)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X67),size(X67),'2341',2.000,
     & X67,S139)
       deallocate(S139)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S140(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S140)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X67),size(X67),'2341',1.000,
     & X67,S140)
       deallocate(S140)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S141(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S141)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X67),size(X67),'2341',1.000,
     & X67,S141)
       deallocate(S141)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S142(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S142)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X67),size(X67),'2341',2.000,
     & X67,S142)
       deallocate(S142)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S143(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S143)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X67),size(X67),'2341',1.000,
     & X67,S143)
       deallocate(S143)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(S144(M1+1:N2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S144)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X18(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       X18=0.0d0
       call sum_stripe(4,shape(X18),size(X18),'3412',0.500,
     & X18,S144)
       deallocate(S144)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(S145(M1+1:N2,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S145)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X19(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       X19=0.0d0
       call sum_stripe(4,shape(X19),size(X19),'3412',0.500,
     & X19,S145)
       deallocate(S145)
C
       allocate(D1(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(S146(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S146)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X20(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       X20=0.0d0
       call sum_stripe(4,shape(X20),size(X20),'3412',0.500,
     & X20,S146)
       deallocate(S146)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(S147(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S147)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X68(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X68=0.0d0
       call sum_stripe(4,shape(X68),size(X68),'2341',1.000,
     & X68,S147)
       deallocate(S147)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(S148(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S148)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X68),size(X68),'2341',1.000,
     & X68,S148)
       deallocate(S148)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(S149(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S149)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X68),size(X68),'2341',2.000,
     & X68,S149)
       deallocate(S149)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(S150(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S150)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X68),size(X68),'2341',2.000,
     & X68,S150)
       deallocate(S150)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(S151(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S151)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X68),size(X68),'2341',1.000,
     & X68,S151)
       deallocate(S151)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(S152(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S152)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X68),size(X68),'2341',1.000,
     & X68,S152)
       deallocate(S152)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z228(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X68,D2,Z228)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & -0.500,V3D,Z228)
       call sum_stripe(6,shape(V3D),size(V3D),'125346',
     & 0.500,V3D,Z228)
       call sum_stripe(6,shape(V3D),size(V3D),'124356',
     & -0.500,V3D,Z228)
       deallocate(Z228)
       deallocate(X68)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(S153(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S153)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',0.500,
     & X65,S153)
C
       allocate(X69(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X69=0.0d0
       call sum_stripe(4,shape(X69),size(X69),'2341',1.000,
     & X69,S153)
       deallocate(S153)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(S155(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S155)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',1.000,
     & X65,S155)
C
       call sum_stripe(4,shape(X69),size(X69),'2341',2.000,
     & X69,S155)
       deallocate(S155)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(S157(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S157)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',0.500,
     & X65,S157)
C
       call sum_stripe(4,shape(X69),size(X69),'2341',1.000,
     & X69,S157)
       deallocate(S157)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S159(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S159)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X42),size(X42),'3412',1.000,
     & X42,S159)
       deallocate(S159)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S160(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S160)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X43),size(X43),'3412',1.000,
     & X43,S160)
       deallocate(S160)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S161(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S161)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X44),size(X44),'3412',1.000,
     & X44,S161)
       deallocate(S161)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S162(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S162)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X45),size(X45),'3412',1.000,
     & X45,S162)
       deallocate(S162)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(S154(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S154)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',0.500,
     & X65,S154)
C
       call sum_stripe(4,shape(X69),size(X69),'2341',1.000,
     & X69,S154)
       deallocate(S154)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(S156(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S156)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',1.000,
     & X65,S156)
C
       call sum_stripe(4,shape(X69),size(X69),'2341',2.000,
     & X69,S156)
       deallocate(S156)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(S158(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S158)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',0.500,
     & X65,S158)
C
       call sum_stripe(4,shape(X69),size(X69),'2341',1.000,
     & X69,S158)
       deallocate(S158)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z195(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X65,D2,Z195)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'236145',
     & 1.000,V3D,Z195)
       call sum_stripe(6,shape(V3D),size(V3D),'235146',
     & -1.000,V3D,Z195)
       call sum_stripe(6,shape(V3D),size(V3D),'234156',
     & 1.000,V3D,Z195)
       deallocate(Z195)
       deallocate(X65)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z234(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X69,D2,Z234)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & -0.500,V3D,Z234)
       call sum_stripe(6,shape(V3D),size(V3D),'135246',
     & 0.500,V3D,Z234)
       call sum_stripe(6,shape(V3D),size(V3D),'134256',
     & -0.500,V3D,Z234)
       deallocate(Z234)
       deallocate(X69)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S163(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S163)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X46),size(X46),'3412',1.000,
     & X46,S163)
       deallocate(S163)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S164(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S164)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X47),size(X47),'3412',1.000,
     & X47,S164)
       deallocate(S164)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S165(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S165)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X48),size(X48),'3412',1.000,
     & X48,S165)
       deallocate(S165)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S166(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S166)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X49),size(X49),'3412',1.000,
     & X49,S166)
       deallocate(S166)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S167(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S167)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S167),size(S167),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,M1-M1/),'3412',S167,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z254(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z254)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z254)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z254)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z254)
       deallocate(Z254)
       deallocate(S167)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S168(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S168)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S168),size(S168),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,M1-M1/),'3412',S168,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z255(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z255)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z255)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z255)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z255)
       deallocate(Z255)
       deallocate(S168)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S169(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S169)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S169),size(S169),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S169,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z256(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z256)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z256)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z256)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z256)
       deallocate(Z256)
       deallocate(S169)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S170(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S170)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S170),size(S170),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N2-N2,M1-M1/),'3412',S170,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z257(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z257)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z257)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z257)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z257)
       deallocate(Z257)
       deallocate(S170)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Q25(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q25)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X12),size(X12),'21',0.500,
     & X12,Q25)
       deallocate(Q25)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Q26(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q26)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X13),size(X13),'21',0.500,
     & X13,Q26)
       deallocate(Q26)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S171(N2+1:M2,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S171)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S171),size(S171),shape(D1),size(D1),
     & (/M2-M2,M2-M2,N2-N2,N2-N2/),'3412',S171,D1)
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,M2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z260(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,D1,F2,Z260)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345612',
     & 0.250,V3D,Z260)
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & -0.250,V3D,Z260)
       deallocate(Z260)
       deallocate(S171)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S172(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S172)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S172),size(S172),shape(D1),size(D1),
     & (/M2-M2,N2-N2,N2-N2,N2-N2/),'3412',S172,D1)
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z261(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K0*K6
       call EGEMM(I1,I2,I3,D1,F2,Z261)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345612',
     & 0.500,V3D,Z261)
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & -0.500,V3D,Z261)
       deallocate(Z261)
       deallocate(S172)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S173(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S173)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S173),size(S173),shape(D1),size(D1),
     & (/N2-N2,N2-N2,N2-N2,N2-N2/),'3412',S173,D1)
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z262(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K0*K0
       call EGEMM(I1,I2,I3,D1,F2,Z262)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345612',
     & 0.250,V3D,Z262)
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & -0.250,V3D,Z262)
       deallocate(Z262)
       deallocate(S173)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(Q27(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q27)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X14(M2+1:N3,N2+1:M2))
       X14=0.0d0
       call sum_stripe(2,shape(X14),size(X14),'21',-0.500,
     & X14,Q27)
       deallocate(Q27)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & M2,N3,N2,M2,X14,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z15(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,X14,F2,Z15)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234561',
     & 1.000,V3D,Z15)
       deallocate(Z15)
       deallocate(X14)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(Q28(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q28)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X15(N2+1:M2,N2+1:M2))
       X15=0.0d0
       call sum_stripe(2,shape(X15),size(X15),'21',-0.500,
     & X15,Q28)
       deallocate(Q28)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & N2,M2,N2,M2,X15,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z16(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K0
       call EGEMM(I1,I2,I3,X15,F2,Z16)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234561',
     & 1.000,V3D,Z16)
       deallocate(Z16)
       deallocate(X15)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S174(N2+1:M2,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S174)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S174),size(S174),shape(D1),size(D1),
     & (/M2-M2,M2-M2,N2-N2,N2-N2/),'3412',S174,D1)
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,M2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z265(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,D1,F2,Z265)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'145623',
     & 0.250,V3D,Z265)
       deallocate(Z265)
       deallocate(S174)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S175(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S175)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S175),size(S175),shape(D1),size(D1),
     & (/M2-M2,N2-N2,N2-N2,N2-N2/),'3412',S175,D1)
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z266(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K0*K6
       call EGEMM(I1,I2,I3,D1,F2,Z266)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'145623',
     & 0.500,V3D,Z266)
       deallocate(Z266)
       deallocate(S175)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S176(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S176)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S176),size(S176),shape(D1),size(D1),
     & (/N2-N2,N2-N2,N2-N2,N2-N2/),'3412',S176,D1)
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z267(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K0*K0
       call EGEMM(I1,I2,I3,D1,F2,Z267)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'145623',
     & 0.250,V3D,Z267)
       deallocate(Z267)
       deallocate(S176)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(Q29(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q29)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X16),size(X16),'21',-0.500,
     & X16,Q29)
       deallocate(Q29)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(Q30(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q30)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X17),size(X17),'21',-0.500,
     & X17,Q30)
       deallocate(Q30)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N2/),'3421',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(Q31(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q31)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X23),size(X23),'21',0.500,
     & X23,Q31)
       deallocate(Q31)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z35(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,X23,F2,Z35)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124563',
     & 1.000,V3D,Z35)
       deallocate(Z35)
       deallocate(X23)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(Q32(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q32)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X24),size(X24),'21',0.500,
     & X24,Q32)
       deallocate(Q32)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z36(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K0
       call EGEMM(I1,I2,I3,X24,F2,Z36)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124563',
     & 1.000,V3D,Z36)
       deallocate(Z36)
       deallocate(X24)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q33(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q33)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q33),size(Q33),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q33,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q34(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q34)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X21),size(X21),'21',1.000,
     & X21,Q34)
       deallocate(Q34)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z31(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K8*K0*K0*K0
       I3=K5
       call EGEMM(I1,I2,I3,X21,F2,Z31)
       deallocate(F2)
C
       V3D=V3D-Z31
       call sum_stripe(6,shape(V3D),size(V3D),'123465',
     & 1.000,V3D,Z31)
       call sum_stripe(6,shape(V3D),size(V3D),'123564',
     & -1.000,V3D,Z31)
       deallocate(Z31)
       deallocate(X21)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q35(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(Q35),size(Q35),shape(B1),size(B1),
     & (/N2-N2,M1-M1/),'21',Q35,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q36(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q36)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X22),size(X22),'21',1.000,
     & X22,Q36)
       deallocate(Q36)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z32(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K8*K0*K0*K0
       I3=K8
       call EGEMM(I1,I2,I3,X22,F2,Z32)
       deallocate(F2)
C
       V3D=V3D-Z32
       call sum_stripe(6,shape(V3D),size(V3D),'123465',
     & 1.000,V3D,Z32)
       call sum_stripe(6,shape(V3D),size(V3D),'123564',
     & -1.000,V3D,Z32)
       deallocate(Z32)
       deallocate(X22)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q37(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q41(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q37,B2,Q41)
       deallocate(B2)
C
       call sum_stripe(2,shape(X59),size(X59),'21',1.000,
     & X59,Q41)
       deallocate(Q41)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z153(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,X59,F2,Z153)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134562',
     & 1.000,V3D,Z153)
       deallocate(Z153)
       deallocate(X59)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q38(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q37,B2,Q38)
       deallocate(B2)
C
       call sum_stripe(2,shape(X57),size(X57),'21',1.000,
     & X57,Q38)
       deallocate(Q38)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z151(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,X57,F2,Z151)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234561',
     & -1.000,V3D,Z151)
       call sum_stripe(6,shape(V3D),size(V3D),'124563',
     & -1.000,V3D,Z151)
       deallocate(Z151)
       deallocate(X57)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q39(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q42(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q39,B2,Q42)
       deallocate(B2)
C
       call sum_stripe(2,shape(X60),size(X60),'21',1.000,
     & X60,Q42)
       deallocate(Q42)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z154(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K0
       call EGEMM(I1,I2,I3,X60,F2,Z154)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134562',
     & 1.000,V3D,Z154)
       deallocate(Z154)
       deallocate(X60)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q40(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q39,B2,Q40)
       deallocate(B2)
C
       call sum_stripe(2,shape(X58),size(X58),'21',1.000,
     & X58,Q40)
       deallocate(Q40)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z152(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K0
       call EGEMM(I1,I2,I3,X58,F2,Z152)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234561',
     & -1.000,V3D,Z152)
       call sum_stripe(6,shape(V3D),size(V3D),'124563',
     & -1.000,V3D,Z152)
       deallocate(Z152)
       deallocate(X58)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q43(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q43),size(Q43),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q43,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(S178(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S178)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S178)
       deallocate(S178)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q43),size(Q43),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q43,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(S177(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S177)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S177)
       deallocate(S177)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M1-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S179(M1+1:N2,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S179)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S179),size(S179),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'2314',S179,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S181(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S181)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X29),size(X29),'2134',-1.000,
     & X29,S181)
       deallocate(S181)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S179),size(S179),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3214',S179,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S182(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S182)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X28),size(X28),'2134',1.000,
     & X28,S182)
       deallocate(S182)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S179),size(S179),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'2314',S179,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S180(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S180)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X27),size(X27),'2134',-1.000,
     & X27,S180)
       deallocate(S180)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S183(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S183)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S183),size(S183),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2314',S183,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S184(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S184)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'3124',-1.000,
     & X3,S184)
C
       call sum_stripe(4,shape(X30),size(X30),'2134',-1.000,
     & X30,S184)
       deallocate(S184)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S185(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S185)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S185),size(S185),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2314',S185,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S186(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S186)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X31),size(X31),'2134',1.000,
     & X31,S186)
       deallocate(S186)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'2413',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S187(M1+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S187)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S187),size(S187),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2341',S187,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S191(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S191)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',1.000,X3,
     & S191)
C
       call sum_stripe(4,shape(X30),size(X30),'3124',1.000,
     & X30,S191)
       deallocate(S191)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S187),size(S187),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2341',S187,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S192(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S192)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X31),size(X31),'2134',1.000,
     & X31,S192)
C
       call sum_stripe(4,shape(X31),size(X31),'3124',-1.000,
     & X31,S192)
       deallocate(S192)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S187),size(S187),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,M1-M1/),'3241',S187,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S188(M1+1:N2,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S188)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'3124',1.000,X1,
     & S188)
C
       call sum_stripe(4,shape(X25),size(X25),'3124',-1.000,
     & X25,S188)
       deallocate(S188)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'2413',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S189(M1+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S189)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S189),size(S189),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2341',S189,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S193(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S193)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'3124',-1.000,
     & X3,S193)
C
       call sum_stripe(4,shape(X30),size(X30),'2134',-1.000,
     & X30,S193)
       deallocate(S193)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S189),size(S189),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,M1-M1/),'3241',S189,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S190(M1+1:N2,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S190)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X26),size(X26),'3124',-1.000,
     & X26,S190)
       deallocate(S190)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S194(M1+1:N2,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S194)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S194),size(S194),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,M1-M1/),'2341',S194,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S202(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S202)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X34),size(X34),'3124',1.000,
     & X34,S202)
       deallocate(S202)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z56(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X34,F2,Z56)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & 1.000,V3D,Z56)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z56)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z56)
       deallocate(Z56)
       deallocate(X34)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S194),size(S194),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,M1-M1/),'2341',S194,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S195(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S195)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'3124',-1.000,
     & X4,S195)
C
       call sum_stripe(4,shape(X32),size(X32),'3124',1.000,
     & X32,S195)
       deallocate(S195)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,M1,N2,X4,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z5(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X4,F2,Z5)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z5)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z5)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z5)
       deallocate(Z5)
       deallocate(X4)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z60(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X32,F2,Z60)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z60)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z60)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z60)
       deallocate(Z60)
       deallocate(X32)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S200(M1+1:N2,N0+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S200)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S200),size(S200),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'2341',S200,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S205(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S205)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X37),size(X37),'3124',1.000,
     & X37,S205)
       deallocate(S205)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z59(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X37,F2,Z59)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & 1.000,V3D,Z59)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z59)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z59)
       deallocate(Z59)
       deallocate(X37)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S200),size(S200),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'2341',S200,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S201(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S201)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'3124',-1.000,
     & X7,S201)
C
       call sum_stripe(4,shape(X39),size(X39),'3124',1.000,
     & X39,S201)
       deallocate(S201)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,M1,N2,X7,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z8(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X7,F2,Z8)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z8)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z8)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z8)
       deallocate(Z8)
       deallocate(X7)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z63(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X39,F2,Z63)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z63)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z63)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z63)
       deallocate(Z63)
       deallocate(X39)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S198(M1+1:N2,N0+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S198)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S198),size(S198),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'2341',S198,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S199(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S199)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'3124',-1.000,
     & X6,S199)
C
       call sum_stripe(4,shape(X33),size(X33),'3124',1.000,
     & X33,S199)
       deallocate(S199)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,N2,M2,M1,N2,X6,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z7(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,X6,F2,Z7)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z7)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z7)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z7)
       deallocate(Z7)
       deallocate(X6)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z62(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,X33,F2,Z62)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z62)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z62)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z62)
       deallocate(Z62)
       deallocate(X33)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S198),size(S198),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'2341',S198,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S204(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S204)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X36),size(X36),'3124',1.000,
     & X36,S204)
       deallocate(S204)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z58(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,X36,F2,Z58)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & 1.000,V3D,Z58)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z58)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z58)
       deallocate(Z58)
       deallocate(X36)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S196(M1+1:N2,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S196)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S196),size(S196),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S196,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S203(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S203)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X35),size(X35),'3124',1.000,
     & X35,S203)
       deallocate(S203)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z57(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X35,F2,Z57)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & 1.000,V3D,Z57)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z57)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z57)
       deallocate(Z57)
       deallocate(X35)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S196),size(S196),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S196,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S197(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S197)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'3124',-1.000,
     & X5,S197)
C
       call sum_stripe(4,shape(X38),size(X38),'3124',1.000,
     & X38,S197)
       deallocate(S197)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,M1,N2,X5,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z6(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X5,F2,Z6)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z6)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z6)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z6)
       deallocate(Z6)
       deallocate(X5)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N2-N2,N2-N2,M1-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z61(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X38,F2,Z61)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z61)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z61)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z61)
       deallocate(Z61)
       deallocate(X38)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'2341',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S206(M1+1:N2,N0+1:M1,N0+1:M1,N2+1:N3))
       I1=K4*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S206)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S206),size(S206),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N0-N0,M1-M1/),'4231',S206,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S207(M1+1:N2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S207)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X18),size(X18),'3124',1.000,
     & X18,S207)
       deallocate(S207)
C
       call sumx_sorted3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,N0,M1,M1,N2,M1,N2,X18,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(Z21(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K0*K0*K0
       I3=K5*K5
       call EGEMM(I1,I2,I3,X18,F2,Z21)
       deallocate(F2)
C
       V3D=V3D+0.500*Z21
       call sum_stripe(6,shape(V3D),size(V3D),'123546',
     & -0.500,V3D,Z21)
       call sum_stripe(6,shape(V3D),size(V3D),'123645',
     & 0.500,V3D,Z21)
       deallocate(Z21)
       deallocate(X18)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N2-N2/),'2341',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S208(M1+1:N2,N0+1:M1,M1+1:N2,N2+1:N3))
       I1=K4*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S208)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S208),size(S208),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M1-M1,M1-M1/),'4231',S208,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S209(M1+1:N2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S209)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X19),size(X19),'3124',1.000,
     & X19,S209)
       deallocate(S209)
C
       call sumx_sorted3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,M1,N2,M1,N2,X19,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(Z22(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K0*K0*K0
       I3=K8*K5
       call EGEMM(I1,I2,I3,X19,F2,Z22)
       deallocate(F2)
C
       V3D=V3D+Z22
       call sum_stripe(6,shape(V3D),size(V3D),'123546',
     & -1.000,V3D,Z22)
       call sum_stripe(6,shape(V3D),size(V3D),'123645',
     & 1.000,V3D,Z22)
       deallocate(Z22)
       deallocate(X19)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,N2-N2/),'2341',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S210(M1+1:N2,M1+1:N2,M1+1:N2,N2+1:N3))
       I1=K4*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S210)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S210),size(S210),shape(D1),size(D1),
     & (/N2-N2,M1-M1,M1-M1,M1-M1/),'4231',S210,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S211(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S211)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X20),size(X20),'3124',1.000,
     & X20,S211)
       deallocate(S211)
C
       call sumx_sorted3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,M1,N2,M1,N2,X20,VCHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(Z23(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K0*K0*K0
       I3=K8*K8
       call EGEMM(I1,I2,I3,X20,F2,Z23)
       deallocate(F2)
C
       V3D=V3D+0.500*Z23
       call sum_stripe(6,shape(V3D),size(V3D),'123546',
     & -0.500,V3D,Z23)
       call sum_stripe(6,shape(V3D),size(V3D),'123645',
     & 0.500,V3D,Z23)
       deallocate(Z23)
       deallocate(X20)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S212(M1+1:N2,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S212)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S212),size(S212),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,M1-M1/),'2341',S212,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S220(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S220)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X46),size(X46),'3124',-1.000,
     & X46,S220)
       deallocate(S220)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z89(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X46,F2,Z89)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z89)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z89)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z89)
       deallocate(Z89)
       deallocate(X46)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S212),size(S212),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,M1-M1/),'2341',S212,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S213(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S213)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X42),size(X42),'3124',-1.000,
     & X42,S213)
       deallocate(S213)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z85(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X42,F2,Z85)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z85)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z85)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z85)
       deallocate(Z85)
       deallocate(X42)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S216(M1+1:N2,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S216)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S216),size(S216),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'2341',S216,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S222(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S222)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X48),size(X48),'3124',-1.000,
     & X48,S222)
       deallocate(S222)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z91(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,X48,F2,Z91)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z91)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z91)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z91)
       deallocate(Z91)
       deallocate(X48)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S216),size(S216),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'2341',S216,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S217(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S217)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X44),size(X44),'3124',-1.000,
     & X44,S217)
       deallocate(S217)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z87(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,X44,F2,Z87)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z87)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z87)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z87)
       deallocate(Z87)
       deallocate(X44)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S214(M1+1:N2,N0+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S214)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S214),size(S214),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S214,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S221(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S221)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X47),size(X47),'3124',-1.000,
     & X47,S221)
       deallocate(S221)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z90(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X47,F2,Z90)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z90)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z90)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z90)
       deallocate(Z90)
       deallocate(X47)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S214),size(S214),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S214,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S215(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S215)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X43),size(X43),'3124',-1.000,
     & X43,S215)
       deallocate(S215)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z86(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X43,F2,Z86)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z86)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z86)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z86)
       deallocate(Z86)
       deallocate(X43)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S218(M1+1:N2,N0+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S218)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S218),size(S218),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'2341',S218,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S223(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S223)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X49),size(X49),'3124',-1.000,
     & X49,S223)
       deallocate(S223)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z92(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,X49,F2,Z92)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z92)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z92)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z92)
       deallocate(Z92)
       deallocate(X49)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S218),size(S218),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'2341',S218,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S219(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S219)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X45),size(X45),'3124',-1.000,
     & X45,S219)
       deallocate(S219)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z88(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,X45,F2,Z88)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z88)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z88)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z88)
       deallocate(Z88)
       deallocate(X45)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S224(M1+1:N2,N0+1:M1,N0+1:N2,M2+1:N3))
       I1=K6*K2*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S224)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S224),size(S224),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,M1-M1/),'3241',S224,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S225(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S225)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X50),size(X50),'3124',-1.000,
     & X50,S225)
       deallocate(S225)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z93(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X50,F2,Z93)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z93)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z93)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z93)
       deallocate(Z93)
       deallocate(X50)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S226(M1+1:N2,M1+1:N2,N0+1:N2,M2+1:N3))
       I1=K6*K2*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S226)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S226),size(S226),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'3241',S226,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S227(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S227)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X51),size(X51),'3124',-1.000,
     & X51,S227)
       deallocate(S227)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z94(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X51,F2,Z94)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z94)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z94)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z94)
       deallocate(Z94)
       deallocate(X51)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S228(M1+1:N2,N0+1:M1,N0+1:N2,N2+1:M2))
       I1=K0*K2*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S228)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S228),size(S228),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'3241',S228,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S229(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S229)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X52),size(X52),'3124',-1.000,
     & X52,S229)
       deallocate(S229)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z95(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,X52,F2,Z95)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z95)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z95)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z95)
       deallocate(Z95)
       deallocate(X52)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S230(M1+1:N2,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S230)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S230),size(S230),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'3241',S230,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S231(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S231)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X53),size(X53),'3124',-1.000,
     & X53,S231)
       deallocate(S231)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z96(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,X53,F2,Z96)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z96)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z96)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z96)
       deallocate(Z96)
       deallocate(X53)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q44(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q44),size(Q44),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q44,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q45(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q45)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X12),size(X12),'21',1.000,
     & X12,Q45)
       deallocate(Q45)
C
       call sumx_sorted21(N0,N2,N0,N2,
     & N0,M1,M1,N2,X12,FBHH, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z13(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K8*K0*K0*K0
       I3=K5
       call EGEMM(I1,I2,I3,X12,F2,Z13)
       deallocate(F2)
C
       V3D=V3D-Z13
       call sum_stripe(6,shape(V3D),size(V3D),'123465',
     & 1.000,V3D,Z13)
       call sum_stripe(6,shape(V3D),size(V3D),'123564',
     & -1.000,V3D,Z13)
       deallocate(Z13)
       deallocate(X12)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q46(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(Q46),size(Q46),shape(B1),size(B1),
     & (/N2-N2,M1-M1/),'21',Q46,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q47(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q47)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X13),size(X13),'21',1.000,
     & X13,Q47)
       deallocate(Q47)
C
       call sumx_sorted21(N0,N2,N0,N2,
     & M1,N2,M1,N2,X13,FBHH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z14(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K8*K0*K0*K0
       I3=K8
       call EGEMM(I1,I2,I3,X13,F2,Z14)
       deallocate(F2)
C
       V3D=V3D-Z14
       call sum_stripe(6,shape(V3D),size(V3D),'123465',
     & 1.000,V3D,Z14)
       call sum_stripe(6,shape(V3D),size(V3D),'123564',
     & -1.000,V3D,Z14)
       deallocate(Z14)
       deallocate(X13)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S232(N2+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S232)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S232),size(S232),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N2-N2/),'2341',S232,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S233(N2+1:M2,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S233)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X54),size(X54),'3124',-1.000,
     & X54,S233)
       deallocate(S233)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,M2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z107(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,X54,F2,Z107)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345612',
     & -0.500,V3D,Z107)
       call sum_stripe(6,shape(V3D),size(V3D),'145632',
     & 0.500,V3D,Z107)
       deallocate(Z107)
       deallocate(X54)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S234(N2+1:M2,N0+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S234)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S234),size(S234),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,N2-N2/),'2341',S234,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S235(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S235)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X55),size(X55),'3124',-1.000,
     & X55,S235)
       deallocate(S235)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z108(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K0*K6
       call EGEMM(I1,I2,I3,X55,F2,Z108)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345612',
     & -1.000,V3D,Z108)
       call sum_stripe(6,shape(V3D),size(V3D),'145632',
     & 1.000,V3D,Z108)
       deallocate(Z108)
       deallocate(X55)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S236(N2+1:M2,N0+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S236)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S236),size(S236),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2341',S236,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S237(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S237)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X56),size(X56),'3124',-1.000,
     & X56,S237)
       deallocate(S237)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z109(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K0*K0
       call EGEMM(I1,I2,I3,X56,F2,Z109)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345612',
     & -0.500,V3D,Z109)
       call sum_stripe(6,shape(V3D),size(V3D),'145632',
     & 0.500,V3D,Z109)
       deallocate(Z109)
       deallocate(X56)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S238(N2+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S238)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S238),size(S238),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N2-N2/),'2341',S238,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S239(N2+1:M2,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S239)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S239),size(S239),shape(D1),size(D1),
     & (/M2-M2,M2-M2,N2-N2,N2-N2/),'2314',S239,D1)
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,M2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z327(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,D1,F2,Z327)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & -0.500,V3D,Z327)
       deallocate(Z327)
       deallocate(S239)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S240(N2+1:M2,N0+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S240)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S240),size(S240),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,N2-N2/),'2341',S240,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S241(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S241)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S241),size(S241),shape(D1),size(D1),
     & (/M2-M2,N2-N2,N2-N2,N2-N2/),'2314',S241,D1)
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z328(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K0*K6
       call EGEMM(I1,I2,I3,D1,F2,Z328)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & -1.000,V3D,Z328)
       deallocate(Z328)
       deallocate(S241)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S242(N2+1:M2,N0+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S242)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S242),size(S242),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2341',S242,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S243(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S243)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S243),size(S243),shape(D1),size(D1),
     & (/N2-N2,N2-N2,N2-N2,N2-N2/),'2314',S243,D1)
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z329(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K0*K0
       call EGEMM(I1,I2,I3,D1,F2,Z329)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & -0.500,V3D,Z329)
       deallocate(Z329)
       deallocate(S243)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q48(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q52(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q48,B2,Q52)
       deallocate(B2)
C
       call sum_stripe(2,shape(X16),size(X16),'21',-1.000,
     & X16,Q52)
       deallocate(Q52)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & M2,N3,N2,M2,X16,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z17(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,X16,F2,Z17)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134562',
     & -1.000,V3D,Z17)
       deallocate(Z17)
       deallocate(X16)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q49(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q48,B2,Q49)
       deallocate(B2)
C
       call sum_stripe(2,shape(X40),size(X40),'21',1.000,
     & X40,Q49)
       deallocate(Q49)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z78(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,X40,F2,Z78)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234561',
     & -1.000,V3D,Z78)
       call sum_stripe(6,shape(V3D),size(V3D),'124563',
     & -1.000,V3D,Z78)
       deallocate(Z78)
       deallocate(X40)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q50(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q50)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q53(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q50,B2,Q53)
       deallocate(B2)
C
       call sum_stripe(2,shape(X17),size(X17),'21',-1.000,
     & X17,Q53)
       deallocate(Q53)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & N2,M2,N2,M2,X17,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z18(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K0
       call EGEMM(I1,I2,I3,X17,F2,Z18)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134562',
     & -1.000,V3D,Z18)
       deallocate(Z18)
       deallocate(X17)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q51(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q50,B2,Q51)
       deallocate(B2)
C
       call sum_stripe(2,shape(X41),size(X41),'21',1.000,
     & X41,Q51)
       deallocate(Q51)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z79(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K0
       call EGEMM(I1,I2,I3,X41,F2,Z79)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234561',
     & -1.000,V3D,Z79)
       call sum_stripe(6,shape(V3D),size(V3D),'124563',
     & -1.000,V3D,Z79)
       deallocate(Z79)
       deallocate(X41)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S244(M1+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S244)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S244),size(S244),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'3421',S244,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S246(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S246)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X29),size(X29),'2314',1.000,
     & X29,S246)
       deallocate(S246)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z41(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X29,D2,Z41)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134256',
     & 1.000,V3D,Z41)
       call sum_stripe(6,shape(V3D),size(V3D),'135246',
     & -1.000,V3D,Z41)
       call sum_stripe(6,shape(V3D),size(V3D),'134265',
     & -1.000,V3D,Z41)
       call sum_stripe(6,shape(V3D),size(V3D),'135264',
     & 1.000,V3D,Z41)
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & 1.000,V3D,Z41)
       call sum_stripe(6,shape(V3D),size(V3D),'136254',
     & -1.000,V3D,Z41)
       deallocate(Z41)
       deallocate(X29)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S244),size(S244),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'3421',S244,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S245(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S245)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X27),size(X27),'2314',1.000,
     & X27,S245)
C
       call sum_stripe(4,shape(X28),size(X28),'2314',1.000,
     & X28,S245)
       deallocate(S245)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z42(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X28,D2,Z42)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124356',
     & -1.000,V3D,Z42)
       call sum_stripe(6,shape(V3D),size(V3D),'125346',
     & 1.000,V3D,Z42)
       call sum_stripe(6,shape(V3D),size(V3D),'124365',
     & 1.000,V3D,Z42)
       call sum_stripe(6,shape(V3D),size(V3D),'125364',
     & -1.000,V3D,Z42)
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & -1.000,V3D,Z42)
       call sum_stripe(6,shape(V3D),size(V3D),'126354',
     & 1.000,V3D,Z42)
       deallocate(Z42)
       deallocate(X28)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S247(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S247)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S247),size(S247),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S247,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S248(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S248)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'3124',1.000,X3,
     & S248)
C
       call sum_stripe(4,shape(X30),size(X30),'2134',1.000,
     & X30,S248)
       deallocate(S248)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S249(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S249)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S249),size(S249),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S249,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S250(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S250)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X31),size(X31),'3124',1.000,
     & X31,S250)
C
       call sum_stripe(4,shape(X31),size(X31),'2134',-1.000,
     & X31,S250)
       deallocate(S250)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S249),size(S249),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S249,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S251(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S251)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',-1.000,
     & X3,S251)
C
       call sum_stripe(4,shape(X30),size(X30),'3124',-1.000,
     & X30,S251)
       deallocate(S251)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S252(M1+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S252)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S252),size(S252),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'2341',S252,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S254(N2+1:M2,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S254)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X61),size(X61),'2314',-1.000,
     & X61,S254)
       deallocate(S254)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z159(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X61,D2,Z159)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'245136',
     & 0.500,V3D,Z159)
       call sum_stripe(6,shape(V3D),size(V3D),'345126',
     & -0.500,V3D,Z159)
       call sum_stripe(6,shape(V3D),size(V3D),'246135',
     & -0.500,V3D,Z159)
       call sum_stripe(6,shape(V3D),size(V3D),'346125',
     & 0.500,V3D,Z159)
       call sum_stripe(6,shape(V3D),size(V3D),'256134',
     & 0.500,V3D,Z159)
       call sum_stripe(6,shape(V3D),size(V3D),'356124',
     & -0.500,V3D,Z159)
       deallocate(Z159)
       deallocate(X61)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S252),size(S252),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'3421',S252,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S255(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S255)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S255),size(S255),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M1-M1,M1-M1/),'3124',S255,D1)
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z345(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,D2,Z345)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'125346',
     & 1.000,V3D,Z345)
       call sum_stripe(6,shape(V3D),size(V3D),'124356',
     & -1.000,V3D,Z345)
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & -1.000,V3D,Z345)
       call sum_stripe(6,shape(V3D),size(V3D),'124365',
     & 1.000,V3D,Z345)
       call sum_stripe(6,shape(V3D),size(V3D),'126354',
     & 1.000,V3D,Z345)
       call sum_stripe(6,shape(V3D),size(V3D),'125364',
     & -1.000,V3D,Z345)
       deallocate(Z345)
       deallocate(S255)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S252),size(S252),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S252,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S256(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S256)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S256),size(S256),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M1-M1,M1-M1/),'3124',S256,D1)
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z346(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,D2,Z346)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'135246',
     & 1.000,V3D,Z346)
       call sum_stripe(6,shape(V3D),size(V3D),'134256',
     & -1.000,V3D,Z346)
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & -1.000,V3D,Z346)
       call sum_stripe(6,shape(V3D),size(V3D),'134265',
     & 1.000,V3D,Z346)
       call sum_stripe(6,shape(V3D),size(V3D),'136254',
     & 1.000,V3D,Z346)
       call sum_stripe(6,shape(V3D),size(V3D),'135264',
     & -1.000,V3D,Z346)
       deallocate(Z346)
       deallocate(S256)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S252),size(S252),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S252,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S257(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S257)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X27),size(X27),'2314',-1.000,
     & X27,S257)
       deallocate(S257)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z40(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X27,D2,Z40)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234156',
     & -1.000,V3D,Z40)
       call sum_stripe(6,shape(V3D),size(V3D),'235146',
     & 1.000,V3D,Z40)
       call sum_stripe(6,shape(V3D),size(V3D),'234165',
     & 1.000,V3D,Z40)
       call sum_stripe(6,shape(V3D),size(V3D),'235164',
     & -1.000,V3D,Z40)
       call sum_stripe(6,shape(V3D),size(V3D),'236145',
     & -1.000,V3D,Z40)
       call sum_stripe(6,shape(V3D),size(V3D),'236154',
     & 1.000,V3D,Z40)
       deallocate(Z40)
       deallocate(X27)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S252),size(S252),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N0-N0,M1-M1/),'4231',S252,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S271(M1+1:N2,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S271)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S271),size(S271),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'2314',S271,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S272(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S272)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',1.000,X1,
     & S272)
       deallocate(S272)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S252),size(S252),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'3241',S252,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S275(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S275)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S275),size(S275),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2314',S275,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S276(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S276)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X30),size(X30),'2134',1.000,
     & X30,S276)
       deallocate(S276)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z43(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X30,D2,Z43)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345126',
     & 1.000,V3D,Z43)
       call sum_stripe(6,shape(V3D),size(V3D),'346125',
     & -1.000,V3D,Z43)
       call sum_stripe(6,shape(V3D),size(V3D),'356124',
     & 1.000,V3D,Z43)
       deallocate(Z43)
       deallocate(X30)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S252),size(S252),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'3241',S252,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S277(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S277)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S277),size(S277),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2314',S277,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S278(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S278)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X31),size(X31),'2134',-1.000,
     & X31,S278)
       deallocate(S278)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z46(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X31,D2,Z46)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'245136',
     & 1.000,V3D,Z46)
       call sum_stripe(6,shape(V3D),size(V3D),'246135',
     & -1.000,V3D,Z46)
       call sum_stripe(6,shape(V3D),size(V3D),'256134',
     & 1.000,V3D,Z46)
       deallocate(Z46)
       deallocate(X31)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S252),size(S252),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'2341',S252,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S253(N2+1:M2,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S253)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',-0.500,
     & X3,S253)
       deallocate(S253)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S271),size(S271),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'2314',S271,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S273(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S273)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X26),size(X26),'2134',-1.000,
     & X26,S273)
       deallocate(S273)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S271),size(S271),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3214',S271,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S274(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S274)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X25),size(X25),'2134',1.000,
     & X25,S274)
       deallocate(S274)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S277),size(S277),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2314',S277,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S279(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S279)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',-1.000,
     & X3,S279)
       deallocate(S279)
C
       call sumx_sorted2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,M2,N2,M2,M1,N2,X3,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z4(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X3,D2,Z4)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'145236',
     & -1.000,V3D,Z4)
       call sum_stripe(6,shape(V3D),size(V3D),'146235',
     & 1.000,V3D,Z4)
       call sum_stripe(6,shape(V3D),size(V3D),'156234',
     & -1.000,V3D,Z4)
       deallocate(Z4)
       deallocate(X3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S265(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S265)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S265),size(S265),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S265,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S268(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S268)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X66),size(X66),'3124',2.000,
     & X66,S268)
       deallocate(S268)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S265),size(S265),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S265,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S266(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S266)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X67),size(X67),'3124',-2.000,
     & X67,S266)
       deallocate(S266)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(S261(M1+1:N2,M1+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S261)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S261),size(S261),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'4312',S261,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S270(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S270)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X25),size(X25),'2134',-0.500,
     & X25,S270)
       deallocate(S270)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z39(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X25,D2,Z39)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124356',
     & 1.000,V3D,Z39)
       call sum_stripe(6,shape(V3D),size(V3D),'125346',
     & -1.000,V3D,Z39)
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & 1.000,V3D,Z39)
       deallocate(Z39)
       deallocate(X25)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S261),size(S261),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3412',S261,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S262(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S262)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-0.500,
     & X1,S262)
       deallocate(S262)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S261),size(S261),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3412',S261,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S267(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S267)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X26),size(X26),'2134',0.500,
     & X26,S267)
       deallocate(S267)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z38(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X26,D2,Z38)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134256',
     & -1.000,V3D,Z38)
       call sum_stripe(6,shape(V3D),size(V3D),'135246',
     & 1.000,V3D,Z38)
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & -1.000,V3D,Z38)
       deallocate(Z38)
       deallocate(X26)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S258(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S258)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S258),size(S258),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S258,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S260(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S260)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X63),size(X63),'3124',-1.000,
     & X63,S260)
C
       call sum_stripe(4,shape(X67),size(X67),'2134',2.000,
     & X67,S260)
       deallocate(S260)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z219(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X67,D2,Z219)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'356124',
     & 0.500,V3D,Z219)
       call sum_stripe(6,shape(V3D),size(V3D),'346125',
     & -0.500,V3D,Z219)
       call sum_stripe(6,shape(V3D),size(V3D),'345126',
     & 0.500,V3D,Z219)
       deallocate(Z219)
       deallocate(X67)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S258),size(S258),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S258,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S263(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S263)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X63),size(X63),'2134',1.000,
     & X63,S263)
       deallocate(S263)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z167(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X63,D2,Z167)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'156234',
     & 1.000,V3D,Z167)
       call sum_stripe(6,shape(V3D),size(V3D),'146235',
     & -1.000,V3D,Z167)
       call sum_stripe(6,shape(V3D),size(V3D),'145236',
     & 1.000,V3D,Z167)
       deallocate(Z167)
       deallocate(X63)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S258),size(S258),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S258,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S259(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S259)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X66),size(X66),'2134',-2.000,
     & X66,S259)
       deallocate(S259)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z213(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X66,D2,Z213)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'256134',
     & 0.500,V3D,Z213)
       call sum_stripe(6,shape(V3D),size(V3D),'246135',
     & -0.500,V3D,Z213)
       call sum_stripe(6,shape(V3D),size(V3D),'245136',
     & 0.500,V3D,Z213)
       deallocate(Z213)
       deallocate(X66)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q54(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q54)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q54),size(Q54),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q54,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(S264(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S264)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S264)
       deallocate(S264)
C
       call sumx_sorted2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,M2,M1,N2,M1,N2,X1,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z1(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X1,D2,Z1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234156',
     & -1.000,V3D,Z1)
       call sum_stripe(6,shape(V3D),size(V3D),'235146',
     & 1.000,V3D,Z1)
       call sum_stripe(6,shape(V3D),size(V3D),'236145',
     & -1.000,V3D,Z1)
       deallocate(Z1)
       deallocate(X1)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q54),size(Q54),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q54,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(S269(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S269)
       deallocate(B1)
       deallocate(D2)
       deallocate(Q54)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S269)
       deallocate(S269)
C
       call sumx_sorted2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,M2,M1,N2,M1,N2,X2,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z2(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X2,D2,Z2)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134256',
     & 1.000,V3D,Z2)
       call sum_stripe(6,shape(V3D),size(V3D),'124356',
     & -1.000,V3D,Z2)
       call sum_stripe(6,shape(V3D),size(V3D),'135246',
     & -1.000,V3D,Z2)
       call sum_stripe(6,shape(V3D),size(V3D),'125346',
     & 1.000,V3D,Z2)
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & 1.000,V3D,Z2)
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & -1.000,V3D,Z2)
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D1(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N2-N2,M1-N0/),'3124',VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z3(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,D1,D2,Z3)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345126',
     & -1.000,V3D,Z3)
       call sum_stripe(6,shape(V3D),size(V3D),'245136',
     & 1.000,V3D,Z3)
       call sum_stripe(6,shape(V3D),size(V3D),'346125',
     & 1.000,V3D,Z3)
       call sum_stripe(6,shape(V3D),size(V3D),'246135',
     & -1.000,V3D,Z3)
       call sum_stripe(6,shape(V3D),size(V3D),'356124',
     & -1.000,V3D,Z3)
       call sum_stripe(6,shape(V3D),size(V3D),'256134',
     & 1.000,V3D,Z3)
       deallocate(Z3)
C
       allocate(B1(M2+1:N3,N2+1:M2))
       call reorder_shift(2,shape(FBPP),size(FBPP),shape(B1),size(B1),
     & (/M2-N2,N2-N2/),'12',FBPP,B1)
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z19(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,B1,F2,Z19)
       deallocate(B1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124563',
     & 1.000,V3D,Z19)
       deallocate(Z19)
C
       allocate(B1(N2+1:M2,N2+1:M2))
       call reorder_shift(2,shape(FBPP),size(FBPP),shape(B1),size(B1),
     & (/N2-N2,N2-N2/),'12',FBPP,B1)
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z20(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K0
       I3=K0
       call EGEMM(I1,I2,I3,B1,F2,Z20)
       deallocate(B1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124563',
     & 1.000,V3D,Z20)
       deallocate(Z20)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z24(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z24)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & -1.000,V3D,Z24)
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & 1.000,V3D,Z24)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z24)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & 1.000,V3D,Z24)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z24)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z24)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & -1.000,V3D,Z24)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z24)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z24)
       deallocate(Z24)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z25(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z25)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & -1.000,V3D,Z25)
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & 1.000,V3D,Z25)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z25)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & 1.000,V3D,Z25)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z25)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z25)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & -1.000,V3D,Z25)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z25)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z25)
       deallocate(Z25)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z26(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z26)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & -1.000,V3D,Z26)
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & 1.000,V3D,Z26)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z26)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & 1.000,V3D,Z26)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z26)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z26)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & -1.000,V3D,Z26)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z26)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z26)
       deallocate(Z26)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,M1-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z27(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z27)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & -1.000,V3D,Z27)
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & 1.000,V3D,Z27)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z27)
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & 1.000,V3D,Z27)
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z27)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z27)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & -1.000,V3D,Z27)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z27)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z27)
       deallocate(Z27)
C
       allocate(D1(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCAPPP),size(VCAPPP),shape(D1),
     & size(D1),(/M2-N2,M2-N2,N2-N2,N2-N2/),'1234',VCAPPP,D1)
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,M2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z28(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,D1,F2,Z28)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345612',
     & 0.500,V3D,Z28)
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & -0.500,V3D,Z28)
       call sum_stripe(6,shape(V3D),size(V3D),'145623',
     & 0.500,V3D,Z28)
       deallocate(Z28)
C
       allocate(D1(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCAPPP),size(VCAPPP),shape(D1),
     & size(D1),(/M2-N2,N2-N2,N2-N2,N2-N2/),'1234',VCAPPP,D1)
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z29(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K0*K6
       call EGEMM(I1,I2,I3,D1,F2,Z29)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345612',
     & 1.000,V3D,Z29)
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & -1.000,V3D,Z29)
       call sum_stripe(6,shape(V3D),size(V3D),'145623',
     & 1.000,V3D,Z29)
       deallocate(Z29)
C
       allocate(D1(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCAPPP),size(VCAPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N2-N2,N2-N2/),'1234',VCAPPP,D1)
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,M1-N0,M1-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z30(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K0
       I3=K0*K0
       call EGEMM(I1,I2,I3,D1,F2,Z30)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345612',
     & 0.500,V3D,Z30)
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & -0.500,V3D,Z30)
       call sum_stripe(6,shape(V3D),size(V3D),'145623',
     & 0.500,V3D,Z30)
       deallocate(Z30)
C
       call sumx_sorted3(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,HT3D,V3D,1.0)
       deallocate(V3D)
C
       end
