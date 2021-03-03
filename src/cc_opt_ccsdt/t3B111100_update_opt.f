       subroutine t3B111100_update(N0,N1,N2,N3,HT3B1,shift,
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
       real*8 HT3B1(N2+1:N3,N1+1:N3,N1+1:M2,N0+1:N2,N0+1:N1,M1+1:N1)
C
       real*8,allocatable::V3B(:,:,:,:,:,:)
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
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::U127(:,:,:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q14(:,:)
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
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::S86(:,:,:,:)
       real*8,allocatable::S87(:,:,:,:)
       real*8,allocatable::S88(:,:,:,:)
       real*8,allocatable::S89(:,:,:,:)
       real*8,allocatable::S90(:,:,:,:)
       real*8,allocatable::S91(:,:,:,:)
       real*8,allocatable::S92(:,:,:,:)
       real*8,allocatable::S93(:,:,:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::S94(:,:,:,:)
       real*8,allocatable::S95(:,:,:,:)
       real*8,allocatable::S96(:,:,:,:)
       real*8,allocatable::S97(:,:,:,:)
       real*8,allocatable::S98(:,:,:,:)
       real*8,allocatable::S99(:,:,:,:)
       real*8,allocatable::U186(:,:,:,:,:,:)
       real*8,allocatable::S100(:,:,:,:)
       real*8,allocatable::S101(:,:,:,:)
       real*8,allocatable::S102(:,:,:,:)
       real*8,allocatable::S103(:,:,:,:)
       real*8,allocatable::S104(:,:,:,:)
       real*8,allocatable::S105(:,:,:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::S106(:,:,:,:)
       real*8,allocatable::S107(:,:,:,:)
       real*8,allocatable::S108(:,:,:,:)
       real*8,allocatable::S109(:,:,:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::S110(:,:,:,:)
       real*8,allocatable::S111(:,:,:,:)
       real*8,allocatable::S112(:,:,:,:)
       real*8,allocatable::S113(:,:,:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::Q27(:,:)
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
       real*8,allocatable::S133(:,:,:,:)
       real*8,allocatable::S134(:,:,:,:)
       real*8,allocatable::S135(:,:,:,:)
       real*8,allocatable::S136(:,:,:,:)
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
       real*8,allocatable::S154(:,:,:,:)
       real*8,allocatable::S155(:,:,:,:)
       real*8,allocatable::S156(:,:,:,:)
       real*8,allocatable::S157(:,:,:,:)
       real*8,allocatable::S158(:,:,:,:)
       real*8,allocatable::S159(:,:,:,:)
       real*8,allocatable::S160(:,:,:,:)
       real*8,allocatable::S161(:,:,:,:)
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::S162(:,:,:,:)
       real*8,allocatable::S163(:,:,:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::S164(:,:,:,:)
       real*8,allocatable::S165(:,:,:,:)
       real*8,allocatable::S166(:,:,:,:)
       real*8,allocatable::S167(:,:,:,:)
       real*8,allocatable::S168(:,:,:,:)
       real*8,allocatable::S169(:,:,:,:)
       real*8,allocatable::S170(:,:,:,:)
       real*8,allocatable::S171(:,:,:,:)
       real*8,allocatable::S172(:,:,:,:)
       real*8,allocatable::S173(:,:,:,:)
       real*8,allocatable::S174(:,:,:,:)
       real*8,allocatable::S175(:,:,:,:)
       real*8,allocatable::S176(:,:,:,:)
       real*8,allocatable::S177(:,:,:,:)
       real*8,allocatable::S178(:,:,:,:)
       real*8,allocatable::S179(:,:,:,:)
       real*8,allocatable::S180(:,:,:,:)
       real*8,allocatable::S181(:,:,:,:)
       real*8,allocatable::S182(:,:,:,:)
       real*8,allocatable::S183(:,:,:,:)
       real*8,allocatable::S184(:,:,:,:)
       real*8,allocatable::S185(:,:,:,:)
       real*8,allocatable::S186(:,:,:,:)
       real*8,allocatable::S187(:,:,:,:)
       real*8,allocatable::S188(:,:,:,:)
       real*8,allocatable::S189(:,:,:,:)
       real*8,allocatable::S190(:,:,:,:)
       real*8,allocatable::S191(:,:,:,:)
       real*8,allocatable::S192(:,:,:,:)
       real*8,allocatable::S193(:,:,:,:)
       real*8,allocatable::S194(:,:,:,:)
       real*8,allocatable::S195(:,:,:,:)
       real*8,allocatable::S196(:,:,:,:)
       real*8,allocatable::S197(:,:,:,:)
       real*8,allocatable::S198(:,:,:,:)
       real*8,allocatable::S199(:,:,:,:)
       real*8,allocatable::S200(:,:,:,:)
       real*8,allocatable::S201(:,:,:,:)
       real*8,allocatable::S202(:,:,:,:)
       real*8,allocatable::S203(:,:,:,:)
       real*8,allocatable::S204(:,:,:,:)
       real*8,allocatable::S205(:,:,:,:)
       real*8,allocatable::S206(:,:,:,:)
       real*8,allocatable::S207(:,:,:,:)
       real*8,allocatable::S208(:,:,:,:)
       real*8,allocatable::S209(:,:,:,:)
       real*8,allocatable::S210(:,:,:,:)
       real*8,allocatable::S211(:,:,:,:)
       real*8,allocatable::S212(:,:,:,:)
       real*8,allocatable::S213(:,:,:,:)
       real*8,allocatable::S214(:,:,:,:)
       real*8,allocatable::S215(:,:,:,:)
       real*8,allocatable::S216(:,:,:,:)
       real*8,allocatable::S217(:,:,:,:)
       real*8,allocatable::S218(:,:,:,:)
       real*8,allocatable::S219(:,:,:,:)
       real*8,allocatable::S220(:,:,:,:)
       real*8,allocatable::S221(:,:,:,:)
       real*8,allocatable::S222(:,:,:,:)
       real*8,allocatable::S223(:,:,:,:)
       real*8,allocatable::S224(:,:,:,:)
       real*8,allocatable::S225(:,:,:,:)
       real*8,allocatable::S226(:,:,:,:)
       real*8,allocatable::S227(:,:,:,:)
       real*8,allocatable::S228(:,:,:,:)
       real*8,allocatable::S229(:,:,:,:)
       real*8,allocatable::S230(:,:,:,:)
       real*8,allocatable::S231(:,:,:,:)
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
       real*8,allocatable::S244(:,:,:,:)
       real*8,allocatable::S245(:,:,:,:)
       real*8,allocatable::S246(:,:,:,:)
       real*8,allocatable::S247(:,:,:,:)
       real*8,allocatable::S248(:,:,:,:)
       real*8,allocatable::S249(:,:,:,:)
       real*8,allocatable::S250(:,:,:,:)
       real*8,allocatable::S251(:,:,:,:)
       real*8,allocatable::S252(:,:,:,:)
       real*8,allocatable::S253(:,:,:,:)
       real*8,allocatable::S254(:,:,:,:)
       real*8,allocatable::S255(:,:,:,:)
       real*8,allocatable::S256(:,:,:,:)
       real*8,allocatable::S257(:,:,:,:)
       real*8,allocatable::S258(:,:,:,:)
       real*8,allocatable::S259(:,:,:,:)
       real*8,allocatable::S260(:,:,:,:)
       real*8,allocatable::S261(:,:,:,:)
       real*8,allocatable::S262(:,:,:,:)
       real*8,allocatable::S263(:,:,:,:)
       real*8,allocatable::S264(:,:,:,:)
       real*8,allocatable::S265(:,:,:,:)
       real*8,allocatable::S266(:,:,:,:)
       real*8,allocatable::S267(:,:,:,:)
       real*8,allocatable::S268(:,:,:,:)
       real*8,allocatable::S269(:,:,:,:)
       real*8,allocatable::S270(:,:,:,:)
       real*8,allocatable::S271(:,:,:,:)
       real*8,allocatable::S272(:,:,:,:)
       real*8,allocatable::S273(:,:,:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::S274(:,:,:,:)
       real*8,allocatable::S275(:,:,:,:)
       real*8,allocatable::S276(:,:,:,:)
       real*8,allocatable::S277(:,:,:,:)
       real*8,allocatable::S278(:,:,:,:)
       real*8,allocatable::S279(:,:,:,:)
       real*8,allocatable::S280(:,:,:,:)
       real*8,allocatable::S281(:,:,:,:)
       real*8,allocatable::S282(:,:,:,:)
       real*8,allocatable::S283(:,:,:,:)
       real*8,allocatable::S284(:,:,:,:)
       real*8,allocatable::S285(:,:,:,:)
       real*8,allocatable::S286(:,:,:,:)
       real*8,allocatable::S287(:,:,:,:)
       real*8,allocatable::S288(:,:,:,:)
       real*8,allocatable::S289(:,:,:,:)
       real*8,allocatable::S290(:,:,:,:)
       real*8,allocatable::S291(:,:,:,:)
       real*8,allocatable::S292(:,:,:,:)
       real*8,allocatable::S293(:,:,:,:)
       real*8,allocatable::S294(:,:,:,:)
       real*8,allocatable::S295(:,:,:,:)
       real*8,allocatable::S296(:,:,:,:)
       real*8,allocatable::S297(:,:,:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q36(:,:)
       real*8,allocatable::U402(:,:,:,:,:,:)
       real*8,allocatable::U517(:,:,:,:,:,:)
       real*8,allocatable::S299(:,:,:,:)
       real*8,allocatable::S300(:,:,:,:)
       real*8,allocatable::S301(:,:,:,:)
       real*8,allocatable::S302(:,:,:,:)
       real*8,allocatable::S303(:,:,:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::Q38(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::S304(:,:,:,:)
       real*8,allocatable::S305(:,:,:,:)
       real*8,allocatable::S306(:,:,:,:)
       real*8,allocatable::S307(:,:,:,:)
       real*8,allocatable::S308(:,:,:,:)
       real*8,allocatable::S309(:,:,:,:)
       real*8,allocatable::S310(:,:,:,:)
       real*8,allocatable::S311(:,:,:,:)
       real*8,allocatable::S312(:,:,:,:)
       real*8,allocatable::S313(:,:,:,:)
       real*8,allocatable::S314(:,:,:,:)
       real*8,allocatable::S315(:,:,:,:)
       real*8,allocatable::S316(:,:,:,:)
       real*8,allocatable::S317(:,:,:,:)
       real*8,allocatable::S318(:,:,:,:)
       real*8,allocatable::S319(:,:,:,:)
       real*8,allocatable::S320(:,:,:,:)
       real*8,allocatable::S321(:,:,:,:)
       real*8,allocatable::S322(:,:,:,:)
       real*8,allocatable::S323(:,:,:,:)
       real*8,allocatable::S324(:,:,:,:)
       real*8,allocatable::S325(:,:,:,:)
       real*8,allocatable::S326(:,:,:,:)
       real*8,allocatable::S327(:,:,:,:)
       real*8,allocatable::S328(:,:,:,:)
       real*8,allocatable::S329(:,:,:,:)
       real*8,allocatable::S330(:,:,:,:)
       real*8,allocatable::S331(:,:,:,:)
       real*8,allocatable::S332(:,:,:,:)
       real*8,allocatable::S333(:,:,:,:)
       real*8,allocatable::S334(:,:,:,:)
       real*8,allocatable::S335(:,:,:,:)
       real*8,allocatable::S336(:,:,:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q44(:,:)
       real*8,allocatable::Q45(:,:)
       real*8,allocatable::S337(:,:,:,:)
       real*8,allocatable::S339(:,:,:,:)
       real*8,allocatable::S338(:,:,:,:)
       real*8,allocatable::S340(:,:,:,:)
       real*8,allocatable::S341(:,:,:,:)
       real*8,allocatable::S342(:,:,:,:)
       real*8,allocatable::S346(:,:,:,:)
       real*8,allocatable::S343(:,:,:,:)
       real*8,allocatable::S344(:,:,:,:)
       real*8,allocatable::S347(:,:,:,:)
       real*8,allocatable::S345(:,:,:,:)
       real*8,allocatable::S348(:,:,:,:)
       real*8,allocatable::S350(:,:,:,:)
       real*8,allocatable::S386(:,:,:,:)
       real*8,allocatable::S349(:,:,:,:)
       real*8,allocatable::S351(:,:,:,:)
       real*8,allocatable::S352(:,:,:,:)
       real*8,allocatable::S387(:,:,:,:)
       real*8,allocatable::S353(:,:,:,:)
       real*8,allocatable::S358(:,:,:,:)
       real*8,allocatable::S359(:,:,:,:)
       real*8,allocatable::S360(:,:,:,:)
       real*8,allocatable::S361(:,:,:,:)
       real*8,allocatable::S362(:,:,:,:)
       real*8,allocatable::S363(:,:,:,:)
       real*8,allocatable::S364(:,:,:,:)
       real*8,allocatable::S365(:,:,:,:)
       real*8,allocatable::S366(:,:,:,:)
       real*8,allocatable::S367(:,:,:,:)
       real*8,allocatable::S368(:,:,:,:)
       real*8,allocatable::S369(:,:,:,:)
       real*8,allocatable::S370(:,:,:,:)
       real*8,allocatable::S371(:,:,:,:)
       real*8,allocatable::Q46(:,:)
       real*8,allocatable::Q47(:,:)
       real*8,allocatable::Q48(:,:)
       real*8,allocatable::Q49(:,:)
       real*8,allocatable::S372(:,:,:,:)
       real*8,allocatable::S373(:,:,:,:)
       real*8,allocatable::S374(:,:,:,:)
       real*8,allocatable::S375(:,:,:,:)
       real*8,allocatable::Q50(:,:)
       real*8,allocatable::Q51(:,:)
       real*8,allocatable::Q52(:,:)
       real*8,allocatable::Q54(:,:)
       real*8,allocatable::Q53(:,:)
       real*8,allocatable::S376(:,:,:,:)
       real*8,allocatable::S377(:,:,:,:)
       real*8,allocatable::S378(:,:,:,:)
       real*8,allocatable::S379(:,:,:,:)
       real*8,allocatable::S380(:,:,:,:)
       real*8,allocatable::S384(:,:,:,:)
       real*8,allocatable::S381(:,:,:,:)
       real*8,allocatable::S382(:,:,:,:)
       real*8,allocatable::S385(:,:,:,:)
       real*8,allocatable::S383(:,:,:,:)
       real*8,allocatable::S354(:,:,:,:)
       real*8,allocatable::S355(:,:,:,:)
       real*8,allocatable::S356(:,:,:,:)
       real*8,allocatable::S357(:,:,:,:)
       real*8,allocatable::S388(:,:,:,:)
       real*8,allocatable::S389(:,:,:,:)
       real*8,allocatable::S390(:,:,:,:)
       real*8,allocatable::S391(:,:,:,:)
       real*8,allocatable::S392(:,:,:,:)
       real*8,allocatable::S394(:,:,:,:)
       real*8,allocatable::S393(:,:,:,:)
       real*8,allocatable::S395(:,:,:,:)
       real*8,allocatable::S397(:,:,:,:)
       real*8,allocatable::S487(:,:,:,:)
       real*8,allocatable::S396(:,:,:,:)
       real*8,allocatable::S400(:,:,:,:)
       real*8,allocatable::S401(:,:,:,:)
       real*8,allocatable::S402(:,:,:,:)
       real*8,allocatable::S406(:,:,:,:)
       real*8,allocatable::S403(:,:,:,:)
       real*8,allocatable::S404(:,:,:,:)
       real*8,allocatable::S407(:,:,:,:)
       real*8,allocatable::S405(:,:,:,:)
       real*8,allocatable::S408(:,:,:,:)
       real*8,allocatable::S409(:,:,:,:)
       real*8,allocatable::S410(:,:,:,:)
       real*8,allocatable::S411(:,:,:,:)
       real*8,allocatable::S412(:,:,:,:)
       real*8,allocatable::S413(:,:,:,:)
       real*8,allocatable::S414(:,:,:,:)
       real*8,allocatable::S415(:,:,:,:)
       real*8,allocatable::S416(:,:,:,:)
       real*8,allocatable::S417(:,:,:,:)
       real*8,allocatable::S418(:,:,:,:)
       real*8,allocatable::S419(:,:,:,:)
       real*8,allocatable::S420(:,:,:,:)
       real*8,allocatable::S421(:,:,:,:)
       real*8,allocatable::S422(:,:,:,:)
       real*8,allocatable::S423(:,:,:,:)
       real*8,allocatable::Q55(:,:)
       real*8,allocatable::Q56(:,:)
       real*8,allocatable::Q57(:,:)
       real*8,allocatable::Q58(:,:)
       real*8,allocatable::S424(:,:,:,:)
       real*8,allocatable::S425(:,:,:,:)
       real*8,allocatable::S426(:,:,:,:)
       real*8,allocatable::S427(:,:,:,:)
       real*8,allocatable::S428(:,:,:,:)
       real*8,allocatable::S432(:,:,:,:)
       real*8,allocatable::S429(:,:,:,:)
       real*8,allocatable::S430(:,:,:,:)
       real*8,allocatable::S433(:,:,:,:)
       real*8,allocatable::S431(:,:,:,:)
       real*8,allocatable::Q59(:,:)
       real*8,allocatable::Q60(:,:)
       real*8,allocatable::Q61(:,:)
       real*8,allocatable::Q62(:,:)
       real*8,allocatable::S298(:,:,:,:)
       real*8,allocatable::S434(:,:,:,:)
       real*8,allocatable::S435(:,:,:,:)
       real*8,allocatable::S436(:,:,:,:)
       real*8,allocatable::S437(:,:,:,:)
       real*8,allocatable::S438(:,:,:,:)
       real*8,allocatable::S439(:,:,:,:)
       real*8,allocatable::S440(:,:,:,:)
       real*8,allocatable::S441(:,:,:,:)
       real*8,allocatable::S442(:,:,:,:)
       real*8,allocatable::S443(:,:,:,:)
       real*8,allocatable::S444(:,:,:,:)
       real*8,allocatable::S445(:,:,:,:)
       real*8,allocatable::Q63(:,:)
       real*8,allocatable::Q64(:,:)
       real*8,allocatable::Q65(:,:)
       real*8,allocatable::Q66(:,:)
       real*8,allocatable::Q67(:,:)
       real*8,allocatable::Q68(:,:)
       real*8,allocatable::Q69(:,:)
       real*8,allocatable::Q71(:,:)
       real*8,allocatable::Q70(:,:)
       real*8,allocatable::S446(:,:,:,:)
       real*8,allocatable::S448(:,:,:,:)
       real*8,allocatable::S449(:,:,:,:)
       real*8,allocatable::S450(:,:,:,:)
       real*8,allocatable::S535(:,:,:,:)
       real*8,allocatable::S536(:,:,:,:)
       real*8,allocatable::S538(:,:,:,:)
       real*8,allocatable::S539(:,:,:,:)
       real*8,allocatable::S447(:,:,:,:)
       real*8,allocatable::S537(:,:,:,:)
       real*8,allocatable::Q72(:,:)
       real*8,allocatable::S458(:,:,:,:)
       real*8,allocatable::S463(:,:,:,:)
       real*8,allocatable::S456(:,:,:,:)
       real*8,allocatable::S459(:,:,:,:)
       real*8,allocatable::S460(:,:,:,:)
       real*8,allocatable::S461(:,:,:,:)
       real*8,allocatable::S462(:,:,:,:)
       real*8,allocatable::S453(:,:,:,:)
       real*8,allocatable::S454(:,:,:,:)
       real*8,allocatable::S457(:,:,:,:)
       real*8,allocatable::S466(:,:,:,:)
       real*8,allocatable::S468(:,:,:,:)
       real*8,allocatable::S467(:,:,:,:)
       real*8,allocatable::S464(:,:,:,:)
       real*8,allocatable::S470(:,:,:,:)
       real*8,allocatable::S471(:,:,:,:)
       real*8,allocatable::S472(:,:,:,:)
       real*8,allocatable::S473(:,:,:,:)
       real*8,allocatable::S474(:,:,:,:)
       real*8,allocatable::S465(:,:,:,:)
       real*8,allocatable::S540(:,:,:,:)
       real*8,allocatable::S541(:,:,:,:)
       real*8,allocatable::S545(:,:,:,:)
       real*8,allocatable::S546(:,:,:,:)
       real*8,allocatable::S543(:,:,:,:)
       real*8,allocatable::S544(:,:,:,:)
       real*8,allocatable::S469(:,:,:,:)
       real*8,allocatable::S542(:,:,:,:)
       real*8,allocatable::S547(:,:,:,:)
       real*8,allocatable::S477(:,:,:,:)
       real*8,allocatable::S478(:,:,:,:)
       real*8,allocatable::S483(:,:,:,:)
       real*8,allocatable::S550(:,:,:,:)
       real*8,allocatable::S551(:,:,:,:)
       real*8,allocatable::S485(:,:,:,:)
       real*8,allocatable::S490(:,:,:,:)
       real*8,allocatable::S491(:,:,:,:)
       real*8,allocatable::S492(:,:,:,:)
       real*8,allocatable::S493(:,:,:,:)
       real*8,allocatable::S494(:,:,:,:)
       real*8,allocatable::S495(:,:,:,:)
       real*8,allocatable::S496(:,:,:,:)
       real*8,allocatable::S497(:,:,:,:)
       real*8,allocatable::S498(:,:,:,:)
       real*8,allocatable::S499(:,:,:,:)
       real*8,allocatable::S500(:,:,:,:)
       real*8,allocatable::S501(:,:,:,:)
       real*8,allocatable::S502(:,:,:,:)
       real*8,allocatable::S503(:,:,:,:)
       real*8,allocatable::S504(:,:,:,:)
       real*8,allocatable::S505(:,:,:,:)
       real*8,allocatable::S506(:,:,:,:)
       real*8,allocatable::S507(:,:,:,:)
       real*8,allocatable::Q74(:,:)
       real*8,allocatable::Q75(:,:)
       real*8,allocatable::Q76(:,:)
       real*8,allocatable::Q77(:,:)
       real*8,allocatable::Q78(:,:)
       real*8,allocatable::Q79(:,:)
       real*8,allocatable::Q80(:,:)
       real*8,allocatable::Q81(:,:)
       real*8,allocatable::S508(:,:,:,:)
       real*8,allocatable::S510(:,:,:,:)
       real*8,allocatable::S518(:,:,:,:)
       real*8,allocatable::S519(:,:,:,:)
       real*8,allocatable::S548(:,:,:,:)
       real*8,allocatable::S549(:,:,:,:)
       real*8,allocatable::S509(:,:,:,:)
       real*8,allocatable::Q82(:,:)
       real*8,allocatable::S525(:,:,:,:)
       real*8,allocatable::S515(:,:,:,:)
       real*8,allocatable::S516(:,:,:,:)
       real*8,allocatable::S513(:,:,:,:)
       real*8,allocatable::S514(:,:,:,:)
       real*8,allocatable::S481(:,:,:,:)
       real*8,allocatable::S482(:,:,:,:)
       real*8,allocatable::S520(:,:,:,:)
       real*8,allocatable::S523(:,:,:,:)
       real*8,allocatable::S524(:,:,:,:)
       real*8,allocatable::S479(:,:,:,:)
       real*8,allocatable::S480(:,:,:,:)
       real*8,allocatable::S517(:,:,:,:)
       real*8,allocatable::S526(:,:,:,:)
       real*8,allocatable::S527(:,:,:,:)
       real*8,allocatable::S528(:,:,:,:)
       real*8,allocatable::S531(:,:,:,:)
       real*8,allocatable::S532(:,:,:,:)
       real*8,allocatable::Q83(:,:)
       real*8,allocatable::S534(:,:,:,:)
       real*8,allocatable::S533(:,:,:,:)
       real*8,allocatable::S451(:,:,:,:)
       real*8,allocatable::S455(:,:,:,:)
       real*8,allocatable::S452(:,:,:,:)
       real*8,allocatable::S475(:,:,:,:)
       real*8,allocatable::S476(:,:,:,:)
       real*8,allocatable::Q73(:,:)
       real*8,allocatable::S484(:,:,:,:)
       real*8,allocatable::S486(:,:,:,:)
       real*8,allocatable::S521(:,:,:,:)
       real*8,allocatable::S522(:,:,:,:)
       real*8,allocatable::S398(:,:,:,:)
       real*8,allocatable::S399(:,:,:,:)
       real*8,allocatable::S529(:,:,:,:)
       real*8,allocatable::S530(:,:,:,:)
       real*8,allocatable::S511(:,:,:,:)
       real*8,allocatable::S512(:,:,:,:)
       real*8,allocatable::S488(:,:,:,:)
       real*8,allocatable::S489(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::Z1(:,:,:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::Z2(:,:,:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::Z3(:,:,:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::Z4(:,:,:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::Z5(:,:,:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::Z6(:,:,:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::Z7(:,:,:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::Z8(:,:,:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
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
       real*8,allocatable::X15(:,:)
       real*8,allocatable::Z15(:,:,:,:,:,:)
       real*8,allocatable::X16(:,:)
       real*8,allocatable::Z16(:,:,:,:,:,:)
       real*8,allocatable::X17(:,:)
       real*8,allocatable::Z17(:,:,:,:,:,:)
       real*8,allocatable::X18(:,:)
       real*8,allocatable::Z18(:,:,:,:,:,:)
       real*8,allocatable::X19(:,:)
       real*8,allocatable::Z19(:,:,:,:,:,:)
       real*8,allocatable::X20(:,:)
       real*8,allocatable::Z20(:,:,:,:,:,:)
       real*8,allocatable::X21(:,:)
       real*8,allocatable::Z21(:,:,:,:,:,:)
       real*8,allocatable::X22(:,:)
       real*8,allocatable::Z22(:,:,:,:,:,:)
       real*8,allocatable::X23(:,:)
       real*8,allocatable::Z23(:,:,:,:,:,:)
       real*8,allocatable::X24(:,:,:,:)
       real*8,allocatable::Z24(:,:,:,:,:,:)
       real*8,allocatable::X25(:,:,:,:)
       real*8,allocatable::Z25(:,:,:,:,:,:)
       real*8,allocatable::X26(:,:,:,:)
       real*8,allocatable::Z26(:,:,:,:,:,:)
       real*8,allocatable::X27(:,:,:,:)
       real*8,allocatable::Z27(:,:,:,:,:,:)
       real*8,allocatable::X28(:,:,:,:)
       real*8,allocatable::Z28(:,:,:,:,:,:)
       real*8,allocatable::X29(:,:,:,:)
       real*8,allocatable::Z29(:,:,:,:,:,:)
       real*8,allocatable::X30(:,:,:,:)
       real*8,allocatable::Z30(:,:,:,:,:,:)
       real*8,allocatable::X31(:,:,:,:)
       real*8,allocatable::Z31(:,:,:,:,:,:)
       real*8,allocatable::X32(:,:,:,:)
       real*8,allocatable::Z32(:,:,:,:,:,:)
       real*8,allocatable::X33(:,:,:,:)
       real*8,allocatable::Z33(:,:,:,:,:,:)
       real*8,allocatable::X34(:,:,:,:)
       real*8,allocatable::Z34(:,:,:,:,:,:)
       real*8,allocatable::X35(:,:,:,:)
       real*8,allocatable::Z35(:,:,:,:,:,:)
       real*8,allocatable::X36(:,:,:,:)
       real*8,allocatable::Z36(:,:,:,:,:,:)
       real*8,allocatable::X37(:,:,:,:)
       real*8,allocatable::Z37(:,:,:,:,:,:)
       real*8,allocatable::X38(:,:,:,:)
       real*8,allocatable::Z38(:,:,:,:,:,:)
       real*8,allocatable::X39(:,:,:,:)
       real*8,allocatable::Z39(:,:,:,:,:,:)
       real*8,allocatable::X40(:,:,:,:)
       real*8,allocatable::Z40(:,:,:,:,:,:)
       real*8,allocatable::X41(:,:,:,:)
       real*8,allocatable::Z41(:,:,:,:,:,:)
       real*8,allocatable::X42(:,:,:,:)
       real*8,allocatable::Z42(:,:,:,:,:,:)
       real*8,allocatable::X43(:,:,:,:)
       real*8,allocatable::Z43(:,:,:,:,:,:)
       real*8,allocatable::X44(:,:,:,:)
       real*8,allocatable::Z44(:,:,:,:,:,:)
       real*8,allocatable::X45(:,:,:,:)
       real*8,allocatable::Z45(:,:,:,:,:,:)
       real*8,allocatable::X46(:,:,:,:)
       real*8,allocatable::Z46(:,:,:,:,:,:)
       real*8,allocatable::X47(:,:,:,:)
       real*8,allocatable::Z47(:,:,:,:,:,:)
       real*8,allocatable::X48(:,:,:,:)
       real*8,allocatable::Z48(:,:,:,:,:,:)
       real*8,allocatable::X49(:,:,:,:)
       real*8,allocatable::Z49(:,:,:,:,:,:)
       real*8,allocatable::X50(:,:,:,:)
       real*8,allocatable::Z50(:,:,:,:,:,:)
       real*8,allocatable::X51(:,:,:,:)
       real*8,allocatable::Z51(:,:,:,:,:,:)
       real*8,allocatable::X52(:,:,:,:)
       real*8,allocatable::Z52(:,:,:,:,:,:)
       real*8,allocatable::X53(:,:,:,:)
       real*8,allocatable::Z53(:,:,:,:,:,:)
       real*8,allocatable::X54(:,:,:,:)
       real*8,allocatable::Z54(:,:,:,:,:,:)
       real*8,allocatable::X55(:,:,:,:)
       real*8,allocatable::Z55(:,:,:,:,:,:)
       real*8,allocatable::X56(:,:,:,:)
       real*8,allocatable::Z56(:,:,:,:,:,:)
       real*8,allocatable::X57(:,:,:,:)
       real*8,allocatable::Z57(:,:,:,:,:,:)
       real*8,allocatable::X58(:,:,:,:)
       real*8,allocatable::Z58(:,:,:,:,:,:)
       real*8,allocatable::X59(:,:,:,:)
       real*8,allocatable::Z59(:,:,:,:,:,:)
       real*8,allocatable::X60(:,:,:,:)
       real*8,allocatable::Z60(:,:,:,:,:,:)
       real*8,allocatable::X61(:,:,:,:)
       real*8,allocatable::Z61(:,:,:,:,:,:)
       real*8,allocatable::X62(:,:,:,:)
       real*8,allocatable::Z62(:,:,:,:,:,:)
       real*8,allocatable::Z63(:,:,:,:,:,:)
       real*8,allocatable::Z64(:,:,:,:,:,:)
       real*8,allocatable::X63(:,:,:,:)
       real*8,allocatable::Z70(:,:,:,:,:,:)
       real*8,allocatable::X64(:,:,:,:)
       real*8,allocatable::Z71(:,:,:,:,:,:)
       real*8,allocatable::Z87(:,:,:,:,:,:)
       real*8,allocatable::Z88(:,:,:,:,:,:)
       real*8,allocatable::Z89(:,:,:,:,:,:)
       real*8,allocatable::X65(:,:,:,:)
       real*8,allocatable::Z94(:,:,:,:,:,:)
       real*8,allocatable::X66(:,:,:,:)
       real*8,allocatable::Z95(:,:,:,:,:,:)
       real*8,allocatable::X67(:,:,:,:)
       real*8,allocatable::Z115(:,:,:,:,:,:)
       real*8,allocatable::X68(:,:,:,:)
       real*8,allocatable::Z116(:,:,:,:,:,:)
       real*8,allocatable::X69(:,:,:,:)
       real*8,allocatable::Z117(:,:,:,:,:,:)
       real*8,allocatable::X70(:,:,:,:)
       real*8,allocatable::Z118(:,:,:,:,:,:)
       real*8,allocatable::X71(:,:,:,:)
       real*8,allocatable::Z119(:,:,:,:,:,:)
       real*8,allocatable::X72(:,:,:,:)
       real*8,allocatable::Z120(:,:,:,:,:,:)
       real*8,allocatable::X73(:,:,:,:)
       real*8,allocatable::Z121(:,:,:,:,:,:)
       real*8,allocatable::X74(:,:,:,:)
       real*8,allocatable::Z122(:,:,:,:,:,:)
       real*8,allocatable::X75(:,:,:,:)
       real*8,allocatable::Z123(:,:,:,:,:,:)
       real*8,allocatable::X76(:,:,:,:)
       real*8,allocatable::Z124(:,:,:,:,:,:)
       real*8,allocatable::X77(:,:)
       real*8,allocatable::Z125(:,:,:,:,:,:)
       real*8,allocatable::X78(:,:)
       real*8,allocatable::Z126(:,:,:,:,:,:)
       real*8,allocatable::Z128(:,:,:,:,:,:)
       real*8,allocatable::X79(:,:)
       real*8,allocatable::Z135(:,:,:,:,:,:)
       real*8,allocatable::X80(:,:)
       real*8,allocatable::Z136(:,:,:,:,:,:)
       real*8,allocatable::Z137(:,:,:,:,:,:)
       real*8,allocatable::Z138(:,:,:,:,:,:)
       real*8,allocatable::Z139(:,:,:,:,:,:)
       real*8,allocatable::Z140(:,:,:,:,:,:)
       real*8,allocatable::Z141(:,:,:,:,:,:)
       real*8,allocatable::Z142(:,:,:,:,:,:)
       real*8,allocatable::Z143(:,:,:,:,:,:)
       real*8,allocatable::Z144(:,:,:,:,:,:)
       real*8,allocatable::Z145(:,:,:,:,:,:)
       real*8,allocatable::Z146(:,:,:,:,:,:)
       real*8,allocatable::Z147(:,:,:,:,:,:)
       real*8,allocatable::Z148(:,:,:,:,:,:)
       real*8,allocatable::X81(:,:,:,:,:,:)
       real*8,allocatable::Z187(:,:,:,:,:,:)
       real*8,allocatable::X82(:,:,:,:)
       real*8,allocatable::Z211(:,:,:,:,:,:)
       real*8,allocatable::Z255(:,:,:,:,:,:)
       real*8,allocatable::Z256(:,:,:,:,:,:)
       real*8,allocatable::Z263(:,:,:,:,:,:)
       real*8,allocatable::X83(:,:,:,:)
       real*8,allocatable::Z282(:,:,:,:,:,:)
       real*8,allocatable::X84(:,:,:,:)
       real*8,allocatable::Z283(:,:,:,:,:,:)
       real*8,allocatable::X85(:,:,:,:)
       real*8,allocatable::Z284(:,:,:,:,:,:)
       real*8,allocatable::X86(:,:,:,:)
       real*8,allocatable::Z285(:,:,:,:,:,:)
       real*8,allocatable::X87(:,:,:,:)
       real*8,allocatable::Z286(:,:,:,:,:,:)
       real*8,allocatable::X88(:,:,:,:)
       real*8,allocatable::Z287(:,:,:,:,:,:)
       real*8,allocatable::X89(:,:,:,:)
       real*8,allocatable::Z296(:,:,:,:,:,:)
       real*8,allocatable::X90(:,:,:,:)
       real*8,allocatable::Z298(:,:,:,:,:,:)
       real*8,allocatable::X91(:,:,:,:)
       real*8,allocatable::Z300(:,:,:,:,:,:)
       real*8,allocatable::X92(:,:,:,:)
       real*8,allocatable::Z302(:,:,:,:,:,:)
       real*8,allocatable::X93(:,:,:,:)
       real*8,allocatable::Z303(:,:,:,:,:,:)
       real*8,allocatable::Z390(:,:,:,:,:,:)
       real*8,allocatable::Z391(:,:,:,:,:,:)
       real*8,allocatable::Z392(:,:,:,:,:,:)
       real*8,allocatable::Z393(:,:,:,:,:,:)
       real*8,allocatable::Z403(:,:,:,:,:,:)
C
       allocate(V3B(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       V3B=0.0d0
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N2,M1-N0/),'2431',VBHPPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S1(M1+1:N1,N0+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,M2+1:N3,M1+1:N2,M1+1:N1))
       X1=0.0d0
       call sum_stripe(4,shape(X1),size(X1),'4123',1.000,X1,
     & S1)
       deallocate(S1)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N2,M1-N0/),'4231',VBHPPH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S2(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N1+1:N3,M2+1:N3,M2+1:N3,M1+1:N2))
       X2=0.0d0
       call sum_stripe(4,shape(X2),size(X2),'3124',-1.000,
     & X2,S2)
       deallocate(S2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N2,M1-N0/),'4231',VBHPPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S3(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N1+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       X3=0.0d0
       call sum_stripe(4,shape(X3),size(X3),'3124',-1.000,
     & X3,S3)
       deallocate(S3)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHH),size(VAHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M1-N0,M1-N0/),'3412',VAHHHH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S4(M2+1:N3,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:N1,M2+1:N3,M1+1:N1,M1+1:N1))
       X4=0.0d0
       call sum_stripe(4,shape(X4),size(X4),'2134',-1.000,
     & X4,S4)
       deallocate(S4)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHH),size(VAHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M1-N0,M1-N0/),'4312',VAHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S5(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       X5=0.0d0
       call sum_stripe(4,shape(X5),size(X5),'2134',1.000,X5,
     & S5)
       deallocate(S5)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N1,M1-N0/),'1432',VAHPHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S6(M1+1:N1,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X63(N0+1:N1,M2+1:N3,M1+1:N1,M1+1:N1))
       X63=0.0d0
       call sum_stripe(4,shape(X63),size(X63),'3124',1.000,
     & X63,S6)
       deallocate(S6)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,M1-N0/),'1432',VAHPHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S7(M1+1:N1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X64(N0+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       X64=0.0d0
       call sum_stripe(4,shape(X64),size(X64),'3124',1.000,
     & X64,S7)
       deallocate(S7)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N1,M1-N0/),'4132',VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S8(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N1+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       X6=0.0d0
       call sum_stripe(4,shape(X6),size(X6),'3124',-1.000,
     & X6,S8)
       deallocate(S8)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M1-N0/),'4132',VAHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S9(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'2134',1.000,X6,
     & S9)
       deallocate(S9)
C
       allocate(D1(N1+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAAPPP),size(VAAPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,M2-N1,N1-N1/),'2134',VAAPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S10(M1+1:N1,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'4123',1.000,X6,
     & S10)
       deallocate(S10)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHH),size(VBHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M1-N0,M1-N0/),'4312',VBHHHH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S11(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K2
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:N2,M2+1:N3,M1+1:N2,M1+1:N1))
       X7=0.0d0
       call sum_stripe(4,shape(X7),size(X7),'2134',-1.000,
     & X7,S11)
       deallocate(S11)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHH),size(VBHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M1-N0,M1-N0/),'4312',VBHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S12(N1+1:M2,N0+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:N2,N1+1:M2,M1+1:N2,M1+1:N1))
       X8=0.0d0
       call sum_stripe(4,shape(X8),size(X8),'2134',-1.000,
     & X8,S12)
       deallocate(S12)
C
       allocate(D1(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHPHP),size(VBHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,M1-N0/),'4132',VBHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S13(M2+1:N3,N2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K4
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N2+1:N3,M2+1:N3,M2+1:N3,M1+1:N1))
       X9=0.0d0
       call sum_stripe(4,shape(X9),size(X9),'3124',-1.000,
     & X9,S13)
       deallocate(S13)
C
       allocate(D1(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHPHP),size(VBHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,M1-N0/),'4132',VBHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S14(N1+1:M2,N2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K4
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       X10=0.0d0
       call sum_stripe(4,shape(X10),size(X10),'3124',-1.000,
     & X10,S14)
       deallocate(S14)
C
       allocate(D1(N1+1:N3,N0+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBPHPH),size(VBPHPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N1,M1-N0/),'2341',VBPHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S15(M1+1:N1,N0+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'4123',1.000,X7,
     & S15)
       deallocate(S15)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBPHPH),size(VBPHPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,M1-N0/),'2341',VBPHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S16(M1+1:N1,N0+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'4123',1.000,X8,
     & S16)
       deallocate(S16)
C
       allocate(D1(N1+1:N3,N2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBAPPP),size(VBAPPP),shape(D1),
     & size(D1),(/N1-N1,N2-N2,M2-N2,N1-N1/),'2134',VBAPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S17(M1+1:N1,N2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K4
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'4123',1.000,
     & X10,S17)
       deallocate(S17)
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
       allocate(X15(N0+1:M1,M1+1:N1))
       X15=0.0d0
       call sum_stripe(2,shape(X15),size(X15),'21',1.000,
     & X15,Q1)
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
       allocate(X16(M1+1:N1,M1+1:N1))
       X16=0.0d0
       call sum_stripe(2,shape(X16),size(X16),'21',1.000,
     & X16,Q2)
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
       allocate(X17(M2+1:N3,M2+1:N3))
       X17=0.0d0
       call sum_stripe(2,shape(X17),size(X17),'21',-1.000,
     & X17,Q3)
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
       allocate(X18(N1+1:M2,M2+1:N3))
       X18=0.0d0
       call sum_stripe(2,shape(X18),size(X18),'21',-1.000,
     & X18,Q4)
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
       allocate(X19(N1+1:M2,N1+1:M2))
       X19=0.0d0
       call sum_stripe(2,shape(X19),size(X19),'21',-1.000,
     & X19,Q5)
       deallocate(Q5)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M1-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S18(M1+1:N1,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S18),size(S18),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'2314',S18,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'561234',t3B4,F2)
       allocate(Z87(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K8*K9*K6*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z87)
       deallocate(D1)
       deallocate(F2)
C
       V3B=V3B+0.500*Z87
       call sum_stripe(6,shape(V3B),size(V3B),'123465',
     & -0.500,V3B,Z87)
       deallocate(Z87)
       deallocate(S18)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,M1-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S19(M1+1:N1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S19),size(S19),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M1-M1,M1-M1/),'2314',S19,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,M2-N1,N1-N1,M1-N0/),'561234',t3B1,F2)
       allocate(Z88(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K8*K9*K6*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z88)
       deallocate(D1)
       deallocate(F2)
C
       V3B=V3B+Z88
       call sum_stripe(6,shape(V3B),size(V3B),'123465',
     & -1.000,V3B,Z88)
       deallocate(Z88)
       deallocate(S19)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,M1-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S20(M1+1:N1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S20),size(S20),shape(D1),size(D1),
     & (/M1-M1,M1-M1,M1-M1,M1-M1/),'2314',S20,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,M2-N1,N1-N1,M1-N0/),'561234',t3B1,F2)
       allocate(Z89(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K8*K9*K6*K6
       I3=K7*K7
       call EGEMM(I1,I2,I3,D1,F2,Z89)
       deallocate(D1)
       deallocate(F2)
C
       V3B=V3B+0.500*Z89
       call sum_stripe(6,shape(V3B),size(V3B),'123465',
     & -0.500,V3B,Z89)
       deallocate(Z89)
       deallocate(S20)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S21(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X27(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       X27=0.0d0
       call sum_stripe(4,shape(X27),size(X27),'3124',-1.000,
     & X27,S21)
       deallocate(S21)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S22(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X28(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       X28=0.0d0
       call sum_stripe(4,shape(X28),size(X28),'3124',-1.000,
     & X28,S22)
       deallocate(S22)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S23(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X29(N0+1:M1,N1+1:M2,M2+1:N3,M1+1:N1))
       X29=0.0d0
       call sum_stripe(4,shape(X29),size(X29),'3124',-1.000,
     & X29,S23)
       deallocate(S23)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S24(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X30(M1+1:N1,N1+1:M2,M2+1:N3,M1+1:N1))
       X30=0.0d0
       call sum_stripe(4,shape(X30),size(X30),'3124',-1.000,
     & X30,S24)
       deallocate(S24)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S25(N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X65(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N1))
       X65=0.0d0
       call sum_stripe(4,shape(X65),size(X65),'3124',1.000,
     & X65,S25)
       deallocate(S25)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S26(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X66(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X66=0.0d0
       call sum_stripe(4,shape(X66),size(X66),'3124',1.000,
     & X66,S26)
       deallocate(S26)
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
       X15=X15+Q6
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
       X16=X16+Q7
       deallocate(Q7)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N1,M2-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S27(M1+1:N1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X27),size(X27),'4123',-1.000,
     & X27,S27)
       deallocate(S27)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N1,M2-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S28(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X28),size(X28),'4123',-1.000,
     & X28,S28)
       deallocate(S28)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,M2-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S29(M1+1:N1,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X29),size(X29),'4123',-1.000,
     & X29,S29)
       deallocate(S29)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N1-N1,M2-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S30(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X30),size(X30),'4123',-1.000,
     & X30,S30)
       deallocate(S30)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S31(M1+1:N1,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X31(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N1))
       X31=0.0d0
       call sum_stripe(4,shape(X31),size(X31),'4123',-1.000,
     & X31,S31)
       deallocate(S31)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N1-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S32(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X32(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X32=0.0d0
       call sum_stripe(4,shape(X32),size(X32),'4123',-1.000,
     & X32,S32)
       deallocate(S32)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S33(N1+1:M2,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X33(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       X33=0.0d0
       call sum_stripe(4,shape(X33),size(X33),'4123',-1.000,
     & X33,S33)
       deallocate(S33)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S34(N1+1:M2,N1+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X34(N1+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       X34=0.0d0
       call sum_stripe(4,shape(X34),size(X34),'4123',-1.000,
     & X34,S34)
       deallocate(S34)
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
       X17=X17-Q8
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
       X18=X18-Q9
       deallocate(Q9)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S35(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X33),size(X33),'3124',1.000,
     & X33,S35)
       deallocate(S35)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S36(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X34),size(X34),'3124',1.000,
     & X34,S36)
       deallocate(S36)
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
       X19=X19-Q10
       deallocate(Q10)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M1-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S37(M1+1:N1,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X35(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N1))
       X35=0.0d0
       call sum_stripe(4,shape(X35),size(X35),'4123',1.000,
     & X35,S37)
       deallocate(S37)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,M1-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S38(M1+1:N1,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X36(M1+1:N2,N0+1:M1,M1+1:N2,M1+1:N1))
       X36=0.0d0
       call sum_stripe(4,shape(X36),size(X36),'4123',1.000,
     & X36,S38)
       deallocate(S38)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,M1-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S39(M1+1:N1,N0+1:M1,M1+1:N1,M1+1:N2))
       I1=K8*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X37(N0+1:M1,M1+1:N1,M1+1:N2,M1+1:N1))
       X37=0.0d0
       call sum_stripe(4,shape(X37),size(X37),'4123',1.000,
     & X37,S39)
       deallocate(S39)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,M1-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S40(M1+1:N1,M1+1:N2,M1+1:N1,M1+1:N2))
       I1=K8*K7*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X38(M1+1:N2,M1+1:N1,M1+1:N2,M1+1:N1))
       X38=0.0d0
       call sum_stripe(4,shape(X38),size(X38),'4123',1.000,
     & X38,S40)
       deallocate(S40)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N2,M2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S41(M1+1:N1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X67(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       X67=0.0d0
       call sum_stripe(4,shape(X67),size(X67),'4123',1.000,
     & X67,S41)
       deallocate(S41)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N2,M2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S42(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X68(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       X68=0.0d0
       call sum_stripe(4,shape(X68),size(X68),'4123',1.000,
     & X68,S42)
       deallocate(S42)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,M2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S43(M1+1:N1,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X69(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       X69=0.0d0
       call sum_stripe(4,shape(X69),size(X69),'4123',1.000,
     & X69,S43)
       deallocate(S43)
C
       allocate(D1(N1+1:N3,M1+1:N1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,M2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S44(M1+1:N1,M1+1:N1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X70(M1+1:N1,N2+1:M2,M2+1:N3,M1+1:N1))
       X70=0.0d0
       call sum_stripe(4,shape(X70),size(X70),'4123',1.000,
     & X70,S44)
       deallocate(S44)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,M1-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S45(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X71(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       X71=0.0d0
       call sum_stripe(4,shape(X71),size(X71),'3124',1.000,
     & X71,S45)
       deallocate(S45)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S46(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X72(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       X72=0.0d0
       call sum_stripe(4,shape(X72),size(X72),'3124',1.000,
     & X72,S46)
       deallocate(S46)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S47(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X73(N0+1:M1,N1+1:M2,M2+1:N3,M1+1:N2))
       X73=0.0d0
       call sum_stripe(4,shape(X73),size(X73),'3124',1.000,
     & X73,S47)
       deallocate(S47)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S48(M2+1:N3,M1+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X74(M1+1:N2,N1+1:M2,M2+1:N3,M1+1:N2))
       X74=0.0d0
       call sum_stripe(4,shape(X74),size(X74),'3124',1.000,
     & X74,S48)
       deallocate(S48)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S49(N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X75(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N2))
       X75=0.0d0
       call sum_stripe(4,shape(X75),size(X75),'3124',1.000,
     & X75,S49)
       deallocate(S49)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S50(N1+1:M2,M1+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X76(M1+1:N2,N1+1:M2,N1+1:M2,M1+1:N2))
       X76=0.0d0
       call sum_stripe(4,shape(X76),size(X76),'3124',1.000,
     & X76,S50)
       deallocate(S50)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q11(N0+1:M1,M1+1:N2))
       I1=K8*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X77(N0+1:M1,M1+1:N2))
       X77=0.0d0
       X77=X77+Q11
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q12(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X78(M1+1:N2,M1+1:N2))
       X78=0.0d0
       X78=X78+Q12
       deallocate(Q12)
C
       allocate(D1(M2+1:N3,M2+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/M2-N2,M2-N1,N0-N0,M2-N2/),'1243',VBHPPP,D1)
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M2-N2,M2-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3B1,F2)
       allocate(U127(N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K7*K7*K8*K9
       I3=K6*K6
       call EGEMM(I1,I2,I3,D1,F2,U127)
       deallocate(D1)
       deallocate(F2)
C
       allocate(F1(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(U127),size(U127),shape(F1),size(F1),
     & (/N0-N0,M2-M2,N1-N1,M1-M1,M1-M1,M1-M1/),'561234',U127,F1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Z128(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K8*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,F1,B2,Z128)
       deallocate(F1)
       deallocate(B2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'213456',
     & -1.000,V3B,Z128)
       deallocate(Z128)
       deallocate(U127)
C
       allocate(D1(N0+1:N1,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,M2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S51(M2+1:N3,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K0
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X49(N2+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       X49=0.0d0
       call sum_stripe(4,shape(X49),size(X49),'4123',-1.000,
     & X49,S51)
       deallocate(S51)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,M2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S52(M2+1:N3,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X50(M2+1:N3,N1+1:M2,M2+1:N3,M2+1:N3))
       X50=0.0d0
       call sum_stripe(4,shape(X50),size(X50),'4123',-1.000,
     & X50,S52)
       deallocate(S52)
C
       allocate(D1(N0+1:N1,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S53(M2+1:N3,N2+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K0
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X51(N2+1:M2,N1+1:M2,M2+1:N3,M2+1:N3))
       X51=0.0d0
       call sum_stripe(4,shape(X51),size(X51),'4123',-1.000,
     & X51,S53)
       deallocate(S53)
C
       allocate(D1(N0+1:N1,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,M2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S54(N1+1:M2,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K0
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X52(N2+1:M2,M2+1:N3,M2+1:N3,N1+1:M2))
       X52=0.0d0
       call sum_stripe(4,shape(X52),size(X52),'4123',-1.000,
     & X52,S54)
       deallocate(S54)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,M2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S55(N1+1:M2,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X53(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       X53=0.0d0
       call sum_stripe(4,shape(X53),size(X53),'4123',-1.000,
     & X53,S55)
       deallocate(S55)
C
       allocate(D1(N0+1:N1,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S56(N1+1:M2,N2+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K0
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X54(N2+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       X54=0.0d0
       call sum_stripe(4,shape(X54),size(X54),'4123',-1.000,
     & X54,S56)
       deallocate(S56)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N2,M2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q13(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X79(M2+1:N3,M2+1:N3))
       X79=0.0d0
       X79=X79+Q13
       deallocate(Q13)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,M2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q14(N2+1:M2,M2+1:N3))
       I1=K6*K0
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X80(N2+1:M2,M2+1:N3))
       X80=0.0d0
       X80=X80+Q14
       deallocate(Q14)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S57(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S57),size(S57),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,M1-M1/),'2314',S57,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,M2-M2,N1-N1,M1-M1,M1-N0/),'412356',t3C3,F2)
       allocate(Z137(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z137)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & 1.000,V3B,Z137)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & -1.000,V3B,Z137)
       deallocate(Z137)
       deallocate(S57)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S58(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S58),size(S58),shape(D1),size(D1),
     & (/M1-M1,M2-M2,M2-M2,M1-M1/),'2314',S58,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,M2-M2,N1-N1,M1-M1,M1-N0/),'412356',t3C3,F2)
       allocate(Z138(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z138)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & 1.000,V3B,Z138)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & -1.000,V3B,Z138)
       deallocate(Z138)
       deallocate(S58)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S59(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S59),size(S59),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'2314',S59,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N1-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z139(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z139)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & -1.000,V3B,Z139)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & 1.000,V3B,Z139)
       deallocate(Z139)
       deallocate(S59)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S60(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S60),size(S60),shape(D1),size(D1),
     & (/M1-M1,N2-N2,M2-M2,M1-M1/),'2314',S60,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N1-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z140(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z140)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & -1.000,V3B,Z140)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & 1.000,V3B,Z140)
       deallocate(Z140)
       deallocate(S60)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S61(N1+1:M2,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S61),size(S61),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'2314',S61,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,M2-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z141(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K8*K6*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z141)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124536',
     & 1.000,V3B,Z141)
       call sum_stripe(6,shape(V3B),size(V3B),'124635',
     & -1.000,V3B,Z141)
       deallocate(Z141)
       deallocate(S61)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S62(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S62)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S62),size(S62),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,M1-M1/),'2314',S62,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,M2-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z142(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K8*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z142)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124536',
     & 1.000,V3B,Z142)
       call sum_stripe(6,shape(V3B),size(V3B),'124635',
     & -1.000,V3B,Z142)
       deallocate(Z142)
       deallocate(S62)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N2,M2-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S63(M1+1:N1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S63),size(S63),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,M1-M1/),'2341',S63,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,M2-M2,N1-N1,M1-M1,M1-N0/),'412356',t3C3,F2)
       allocate(Z143(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z143)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & -1.000,V3B,Z143)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & 1.000,V3B,Z143)
       deallocate(Z143)
       deallocate(S63)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N2,M2-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S64(M1+1:N1,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S64),size(S64),shape(D1),size(D1),
     & (/M1-M1,M2-M2,M2-M2,M1-M1/),'2341',S64,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,M2-M2,N1-N1,M1-M1,M1-N0/),'412356',t3C3,F2)
       allocate(Z144(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z144)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & -1.000,V3B,Z144)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & 1.000,V3B,Z144)
       deallocate(Z144)
       deallocate(S64)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,M2-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S65(M1+1:N1,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S65),size(S65),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'2341',S65,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N1-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z145(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z145)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & 1.000,V3B,Z145)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & -1.000,V3B,Z145)
       deallocate(Z145)
       deallocate(S65)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,M2-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S66(M1+1:N1,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S66)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S66),size(S66),shape(D1),size(D1),
     & (/M1-M1,N2-N2,M2-M2,M1-M1/),'2341',S66,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N1-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z146(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z146)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & 1.000,V3B,Z146)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & -1.000,V3B,Z146)
       deallocate(Z146)
       deallocate(S66)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S67(M1+1:N1,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S67),size(S67),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'2341',S67,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,M2-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z147(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K8*K6*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z147)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124536',
     & -1.000,V3B,Z147)
       call sum_stripe(6,shape(V3B),size(V3B),'124635',
     & 1.000,V3B,Z147)
       deallocate(Z147)
       deallocate(S67)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S68(M1+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S68)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S68),size(S68),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,M1-M1/),'2341',S68,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,M2-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z148(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K8*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z148)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124536',
     & -1.000,V3B,Z148)
       call sum_stripe(6,shape(V3B),size(V3B),'124635',
     & 1.000,V3B,Z148)
       deallocate(Z148)
       deallocate(S68)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHH),size(VBHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M1-N0,M1-N0/),'3412',VBHHHH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S69(M2+1:N3,N0+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S69)
       deallocate(S69)
C
       allocate(D1(N2+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHPHP),size(VBHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,M1-N0/),'1432',VBHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S70(M1+1:N2,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S70)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'3124',1.000,X1,
     & S70)
       deallocate(S70)
C
       allocate(D1(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBPHPH),size(VBPHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N1,M1-N0/),'3241',VBPHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S71(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S71)
       deallocate(S71)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBPHPH),size(VBPHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M1-N0/),'3241',VBPHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S72(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S72)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',-1.000,
     & X3,S72)
       deallocate(S72)
C
       allocate(D1(N2+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBAPPP),size(VBAPPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,M2-N2,N1-N1/),'1234',VBAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S73(M1+1:N2,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'4123',1.000,X3,
     & S73)
       deallocate(S73)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N1,M1-N0/),'3124',VBHPPH,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S74(M1+1:N2,N0+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S74)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'3124',1.000,X7,
     & S74)
       deallocate(S74)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,M1-N0/),'3124',VBHPPH,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S75(M1+1:N2,N0+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'3124',1.000,X8,
     & S75)
       deallocate(S75)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,M1-N0/),'1324',VBHPPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S76(M2+1:N3,N2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S76)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',-1.000,
     & X9,S76)
       deallocate(S76)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'1324',VBHPPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S77(M2+1:N3,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',-1.000,
     & X10,S77)
       deallocate(S77)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S78(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S78)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       X11=0.0d0
       call sum_stripe(4,shape(X11),size(X11),'3124',-1.000,
     & X11,S78)
       deallocate(S78)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S79(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N2))
       X12=0.0d0
       call sum_stripe(4,shape(X12),size(X12),'3124',-1.000,
     & X12,S79)
       deallocate(S79)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S80(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S80)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:M1,N1+1:M2,M2+1:N3,M1+1:N2))
       X13=0.0d0
       call sum_stripe(4,shape(X13),size(X13),'3124',-1.000,
     & X13,S80)
       deallocate(S80)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S81(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S81)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(M1+1:N1,N1+1:M2,M2+1:N3,M1+1:N2))
       X14=0.0d0
       call sum_stripe(4,shape(X14),size(X14),'3124',-1.000,
     & X14,S81)
       deallocate(S81)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S82(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S82)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'4123',1.000,
     & X11,S82)
       deallocate(S82)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S83(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'4123',1.000,
     & X12,S83)
       deallocate(S83)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S84(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S84)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X13),size(X13),'4123',1.000,
     & X13,S84)
       deallocate(S84)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S85(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S85)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X14),size(X14),'4123',1.000,
     & X14,S85)
       deallocate(S85)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q15(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q15)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X20(N0+1:M1,M1+1:N2))
       X20=0.0d0
       call sum_stripe(2,shape(X20),size(X20),'21',1.000,
     & X20,Q15)
       deallocate(Q15)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,M1-N0/),'12',FBHP,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q16(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q16)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X21(M1+1:N2,M1+1:N2))
       X21=0.0d0
       call sum_stripe(2,shape(X21),size(X21),'21',1.000,
     & X21,Q16)
       deallocate(Q16)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,M2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q17(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q17)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X22(M2+1:N3,M2+1:N3))
       X22=0.0d0
       call sum_stripe(2,shape(X22),size(X22),'21',-1.000,
     & X22,Q17)
       deallocate(Q17)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,N2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q18(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q18)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X23(N2+1:M2,M2+1:N3))
       X23=0.0d0
       call sum_stripe(2,shape(X23),size(X23),'21',-1.000,
     & X23,Q18)
       deallocate(Q18)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M1-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S86(M1+1:N2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S86)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X35),size(X35),'3124',1.000,
     & X35,S86)
       deallocate(S86)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,M1-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S87(M1+1:N2,M1+1:N2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S87)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X36),size(X36),'3124',1.000,
     & X36,S87)
       deallocate(S87)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M1-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S88(M1+1:N2,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S88)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X37),size(X37),'3124',1.000,
     & X37,S88)
       deallocate(S88)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,M1-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S89(M1+1:N2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S89)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X38),size(X38),'3124',1.000,
     & X38,S89)
       deallocate(S89)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M1-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S90(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X39(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       X39=0.0d0
       call sum_stripe(4,shape(X39),size(X39),'3124',-1.000,
     & X39,S90)
       deallocate(S90)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,M1,N1,X39,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(Z39(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X39,F2,Z39)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'234516',
     & 1.000,V3B,Z39)
       call sum_stripe(6,shape(V3B),size(V3B),'234615',
     & -1.000,V3B,Z39)
       deallocate(Z39)
       deallocate(X39)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S91(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X40(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       X40=0.0d0
       call sum_stripe(4,shape(X40),size(X40),'3124',-1.000,
     & X40,S91)
       deallocate(S91)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M1,N1,X40,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(Z40(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X40,F2,Z40)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'234516',
     & 1.000,V3B,Z40)
       call sum_stripe(6,shape(V3B),size(V3B),'234615',
     & -1.000,V3B,Z40)
       deallocate(Z40)
       deallocate(X40)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S92(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S92)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X41(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       X41=0.0d0
       call sum_stripe(4,shape(X41),size(X41),'3124',-1.000,
     & X41,S92)
       deallocate(S92)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,N2,M2,M2,N3,M1,N1,X41,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(Z41(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X41,F2,Z41)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'234516',
     & 1.000,V3B,Z41)
       call sum_stripe(6,shape(V3B),size(V3B),'234615',
     & -1.000,V3B,Z41)
       deallocate(Z41)
       deallocate(X41)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S93(M2+1:N3,M1+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X42(M1+1:N1,N2+1:M2,M2+1:N3,M1+1:N1))
       X42=0.0d0
       call sum_stripe(4,shape(X42),size(X42),'3124',-1.000,
     & X42,S93)
       deallocate(S93)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,N2,M2,M2,N3,M1,N1,X42,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(Z42(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K0*K7
       call EGEMM(I1,I2,I3,X42,F2,Z42)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'234516',
     & 1.000,V3B,Z42)
       call sum_stripe(6,shape(V3B),size(V3B),'234615',
     & -1.000,V3B,Z42)
       deallocate(Z42)
       deallocate(X42)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q19(N0+1:M1,M1+1:N1))
       I1=K7*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q19)
       deallocate(D1)
       deallocate(B2)
C
       X15=X15+Q19
       deallocate(Q19)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q20(M1+1:N1,M1+1:N1))
       I1=K7*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q20)
       deallocate(D1)
       deallocate(B2)
C
       X16=X16+Q20
       deallocate(Q20)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N1,M2-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S94(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S94)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X43(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       X43=0.0d0
       call sum_stripe(4,shape(X43),size(X43),'4123',1.000,
     & X43,S94)
       deallocate(S94)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,M2,N3,M1,N2,X43,VBPHPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N2,N1-N1,M1-N0,M1-M1/),'421356',t3B1,F2)
       allocate(Z43(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X43,F2,Z43)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'135624',
     & -1.000,V3B,Z43)
       deallocate(Z43)
       deallocate(X43)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,M2-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S95(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X44(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       X44=0.0d0
       call sum_stripe(4,shape(X44),size(X44),'4123',1.000,
     & X44,S95)
       deallocate(S95)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,M2,N3,M1,N2,X44,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N2,N1-N1,M1-N0,M1-M1/),'421356',t3B1,F2)
       allocate(Z44(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X44,F2,Z44)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'135624',
     & -1.000,V3B,Z44)
       deallocate(Z44)
       deallocate(X44)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,M2-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S96(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S96)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X45(N0+1:M1,N1+1:M2,M2+1:N3,M1+1:N2))
       X45=0.0d0
       call sum_stripe(4,shape(X45),size(X45),'4123',1.000,
     & X45,S96)
       deallocate(S96)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,M1,N1,M2,M2,N3,M1,N2,X45,VBPHPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,N1-N1,M1-N0,M1-M1/),'421356',t3B1,F2)
       allocate(Z45(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X45,F2,Z45)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'135624',
     & -1.000,V3B,Z45)
       deallocate(Z45)
       deallocate(X45)
C
       allocate(D1(N2+1:N3,M1+1:N2,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,M2-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S97(M1+1:N2,M1+1:N2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X46(M1+1:N2,N1+1:M2,M2+1:N3,M1+1:N2))
       X46=0.0d0
       call sum_stripe(4,shape(X46),size(X46),'4123',1.000,
     & X46,S97)
       deallocate(S97)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,N1,M2,M2,N3,M1,N2,X46,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,N1-N1,M1-N0,M1-M1/),'421356',t3B1,F2)
       allocate(Z46(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K9*K8
       call EGEMM(I1,I2,I3,X46,F2,Z46)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'135624',
     & -1.000,V3B,Z46)
       deallocate(Z46)
       deallocate(X46)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S98(M1+1:N2,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S98)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X47(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N2))
       X47=0.0d0
       call sum_stripe(4,shape(X47),size(X47),'4123',1.000,
     & X47,S98)
       deallocate(S98)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,M1,N1,M2,N1,M2,M1,N2,X47,VBPHPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,M2-N1,M1-N0,M1-M1/),'431256',t3B1,F2)
       allocate(Z47(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K7*K7*K6*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X47,F2,Z47)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'125634',
     & -1.000,V3B,Z47)
       deallocate(Z47)
       deallocate(X47)
C
       allocate(D1(N2+1:N3,M1+1:N2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S99(M1+1:N2,M1+1:N2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X48(M1+1:N2,N1+1:M2,N1+1:M2,M1+1:N2))
       X48=0.0d0
       call sum_stripe(4,shape(X48),size(X48),'4123',1.000,
     & X48,S99)
       deallocate(S99)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,N1,M2,N1,M2,M1,N2,X48,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,M2-N1,M1-N0,M1-M1/),'431256',t3B1,F2)
       allocate(Z48(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K7*K7*K6*K6
       I3=K9*K8
       call EGEMM(I1,I2,I3,X48,F2,Z48)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'125634',
     & -1.000,V3B,Z48)
       deallocate(Z48)
       deallocate(X48)
C
       allocate(D1(M2+1:N3,M2+1:N3,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/M2-N2,M2-N1,N0-N0,M2-N1/),'1234',VBPHPP,D1)
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M2-N2,M2-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3B1,F2)
       allocate(U186(N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1,N0+1:N2,M2+1:N3))
       I1=K6*K2
       I2=K7*K7*K8*K9
       I3=K6*K6
       call EGEMM(I1,I2,I3,D1,F2,U186)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X81(N0+1:N2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       X81=0.0d0
       call sum_stripe(6,shape(X81),size(X81),'345612',
     & 1.000,X81,U186)
       deallocate(U186)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S100(M2+1:N3,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S100)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X49),size(X49),'3124',-1.000,
     & X49,S100)
       deallocate(S100)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S101(M2+1:N3,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X50),size(X50),'3124',-1.000,
     & X50,S101)
       deallocate(S101)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S102(M2+1:N3,N2+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S102)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X51),size(X51),'3124',-1.000,
     & X51,S102)
       deallocate(S102)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S103(M2+1:N3,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X52),size(X52),'3124',-1.000,
     & X52,S103)
       deallocate(S103)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S104(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S104)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X53),size(X53),'3124',-1.000,
     & X53,S104)
       deallocate(S104)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S105(M2+1:N3,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X54),size(X54),'3124',-1.000,
     & X54,S105)
       deallocate(S105)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q21(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q21)
       deallocate(D1)
       deallocate(B2)
C
       X17=X17+Q21
       deallocate(Q21)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q22(N1+1:M2,M2+1:N3))
       I1=K6*K9
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q22)
       deallocate(D1)
       deallocate(B2)
C
       X18=X18+Q22
       deallocate(Q22)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q23(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q23)
       deallocate(D1)
       deallocate(B2)
C
       X19=X19+Q23
       deallocate(Q23)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S106(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S106)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X55(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       X55=0.0d0
       call sum_stripe(4,shape(X55),size(X55),'3124',-1.000,
     & X55,S106)
       deallocate(S106)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S107(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S107)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X56(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       X56=0.0d0
       call sum_stripe(4,shape(X56),size(X56),'3124',-1.000,
     & X56,S107)
       deallocate(S107)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S108(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S108)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X57(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N2))
       X57=0.0d0
       call sum_stripe(4,shape(X57),size(X57),'3124',-1.000,
     & X57,S108)
       deallocate(S108)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S109(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S109)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X58(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N2))
       X58=0.0d0
       call sum_stripe(4,shape(X58),size(X58),'3124',-1.000,
     & X58,S109)
       deallocate(S109)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q24(N0+1:M1,M1+1:N2))
       I1=K8*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q24)
       deallocate(D1)
       deallocate(B2)
C
       X20=X20+Q24
       deallocate(Q24)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M1-N0/),'3142',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q25(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q25)
       deallocate(D1)
       deallocate(B2)
C
       X21=X21+Q25
       deallocate(Q25)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S110(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S110)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X55),size(X55),'4123',-1.000,
     & X55,S110)
       deallocate(S110)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S111(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S111)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X56),size(X56),'4123',-1.000,
     & X56,S111)
       deallocate(S111)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S112(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S112)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X57),size(X57),'4123',-1.000,
     & X57,S112)
       deallocate(S112)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S113(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S113)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X58),size(X58),'4123',-1.000,
     & X58,S113)
       deallocate(S113)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q26(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q26)
       deallocate(D1)
       deallocate(B2)
C
       X22=X22-Q26
       deallocate(Q26)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q27(N2+1:M2,M2+1:N3))
       I1=K6*K0
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q27)
       deallocate(D1)
       deallocate(B2)
C
       X23=X23-Q27
       deallocate(Q27)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,M2-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S114(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S114)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S114)
       deallocate(S114)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S115(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S115)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S115)
       deallocate(S115)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,M2-N2,M1-N0,M1-N0/),'2134',t2B,D2)
       allocate(S116(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S116)
       deallocate(B1)
       deallocate(D2)
C
       allocate(X82(N0+1:N1,M2+1:N3,M1+1:N2,M1+1:N1))
       X82=0.0d0
       call sum_stripe(4,shape(X82),size(X82),'2341',1.000,
     & X82,S116)
       deallocate(S116)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'3142',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S117(M2+1:N3,M1+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S117)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S117)
       deallocate(S117)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'3142',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S118(M2+1:N3,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S118)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X63),size(X63),'2314',1.000,
     & X63,S118)
       deallocate(S118)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'4132',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S119(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S119)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X64),size(X64),'2314',-1.000,
     & X64,S119)
       deallocate(S119)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S120(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S120)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2314',0.500,X6,
     & S120)
       deallocate(S120)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S121(M2+1:N3,M1+1:N2,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S121)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2413',-1.000,
     & X2,S121)
       deallocate(S121)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,M2-N1/),'1243',VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S122(M1+1:N1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S122)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'3412',0.500,X4,
     & S122)
       deallocate(S122)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S123(M2+1:N3,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S123)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2413',-1.000,
     & X3,S123)
       deallocate(S123)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,N1-N1/),'1243',VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S124(M1+1:N1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S124)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'3412',0.500,X5,
     & S124)
       deallocate(S124)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M2-N1/),'4213',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S125(N1+1:M2,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S125)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'3412',1.000,X6,
     & S125)
       deallocate(S125)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4213',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S126(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S126)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2413',-1.000,
     & X6,S126)
       deallocate(S126)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S127(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S127)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',1.000,X3,
     & S127)
       deallocate(S127)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1/),'3412',t2B,D2)
       allocate(S128(M2+1:N3,M2+1:N3,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K6*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S128)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2314',1.000,X2,
     & S128)
       deallocate(S128)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N2/),'4123',VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S129(N1+1:M2,M1+1:N2,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S129)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'3412',-1.000,
     & X3,S129)
       deallocate(S129)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N2/),'4123',VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'4123',t2B,D2)
       allocate(S130(M2+1:N3,M1+1:N2,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K8*K6
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S130)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'3412',-1.000,
     & X2,S130)
       deallocate(S130)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'3241',VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S131(M2+1:N3,M1+1:N1,N0+1:N1,M1+1:N2))
       I1=K8*K1
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S131)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X82),size(X82),'2413',-1.000,
     & X82,S131)
       deallocate(S131)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S132(M2+1:N3,M1+1:N1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S132)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2413',1.000,X7,
     & S132)
       deallocate(S132)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S133(N1+1:M2,M1+1:N1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S133)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2413',1.000,X8,
     & S133)
       deallocate(S133)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,M2-N2/),'1243',VBHPPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(S134(M1+1:N2,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S134)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X82),size(X82),'3412',1.000,
     & X82,S134)
       deallocate(S134)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,M2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S135(M2+1:N3,M1+1:N1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S135)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'3412',1.000,X9,
     & S135)
       deallocate(S135)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,M2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S136(N1+1:M2,M1+1:N1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S136)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'3412',1.000,
     & X10,S136)
       deallocate(S136)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S137(M2+1:N3,M1+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S137)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S137)
       deallocate(S137)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S138(M2+1:N3,M1+1:N2,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S138)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2413',1.000,X2,
     & S138)
       deallocate(S138)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S139(M2+1:N3,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S139)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2413',1.000,X3,
     & S139)
       deallocate(S139)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,M2-N2,N1-N1,M1-M1/),'562134',t3B4,F2)
       allocate(S140(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S140)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',0.500,X3,
     & S140)
       deallocate(S140)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,M2-N2,N1-N1,M1-N0/),'562134',t3B1,F2)
       allocate(S141(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S141)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',1.000,X3,
     & S141)
       deallocate(S141)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N1,M2-N2,N1-N1,M1-N0/),'562134',t3B1,F2)
       allocate(S142(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S142)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',0.500,X3,
     & S142)
       deallocate(S142)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,M2-N2,N1-N1,M1-M1/),'562134',t3B4,F2)
       allocate(S143(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S143)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',0.500,X3,
     & S143)
       deallocate(S143)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,M2-N2,N1-N1,M1-N0/),'562134',t3B1,F2)
       allocate(S144(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S144)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',1.000,X3,
     & S144)
       deallocate(S144)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N1-N1,M2-N2,N1-N1,M1-N0/),'562134',t3B1,F2)
       allocate(S145(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S145)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',0.500,X3,
     & S145)
       deallocate(S145)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,M2-N2,M2-N1,M1-M1/),'563124',t3B4,F2)
       allocate(S146(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S146)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',0.500,X2,
     & S146)
       deallocate(S146)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,M2-N2,M2-N1,M1-N0/),'563124',t3B1,F2)
       allocate(S147(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S147)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S147)
       deallocate(S147)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N1-N1,M2-N2,M2-N1,M1-N0/),'563124',t3B1,F2)
       allocate(S148(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S148)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',0.500,X2,
     & S148)
       deallocate(S148)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S149(M1+1:N1,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S149)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X24(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N1))
       X24=0.0d0
       call sum_stripe(4,shape(X24),size(X24),'3412',0.500,
     & X24,S149)
       deallocate(S149)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S150(M1+1:N1,M1+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S150)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X25(N0+1:M1,M1+1:N1,M1+1:N1,M1+1:N1))
       X25=0.0d0
       call sum_stripe(4,shape(X25),size(X25),'3412',0.500,
     & X25,S150)
       deallocate(S150)
C
       allocate(D1(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S151(M1+1:N1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S151)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X26(M1+1:N1,M1+1:N1,M1+1:N1,M1+1:N1))
       X26=0.0d0
       call sum_stripe(4,shape(X26),size(X26),'3412',0.500,
     & X26,S151)
       deallocate(S151)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N1-N1,M2-N2,M1-N0,M1-M1/),'523146',t3B1,F2)
       allocate(S152(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S152)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X82),size(X82),'2341',1.000,
     & X82,S152)
       deallocate(S152)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N1-N1,M2-N2,M1-N0,M1-M1/),'523146',t3B1,F2)
       allocate(S153(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S153)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X82),size(X82),'2341',1.000,
     & X82,S153)
       deallocate(S153)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N1-N1,M2-N2,M1-N0,M1-M1/),'523146',t3B1,F2)
       allocate(S154(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S154)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X82),size(X82),'2341',0.500,
     & X82,S154)
       deallocate(S154)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N1-N1,M2-N2,M1-N0,M1-M1/),'523146',t3B1,F2)
       allocate(S155(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S155)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X82),size(X82),'2341',0.500,
     & X82,S155)
       deallocate(S155)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S156(M2+1:N3,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S156)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X27),size(X27),'3412',-1.000,
     & X27,S156)
       deallocate(S156)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S157(M2+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S157)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X28),size(X28),'3412',-1.000,
     & X28,S157)
       deallocate(S157)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S158(M2+1:N3,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S158)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X29),size(X29),'3412',-1.000,
     & X29,S158)
       deallocate(S158)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S159(M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S159)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X30),size(X30),'3412',-1.000,
     & X30,S159)
       deallocate(S159)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S160(N1+1:M2,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S160)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S160),size(S160),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'3412',S160,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,M2-N1,M1-N0,M1-M1/),'531246',t3B1,F2)
       allocate(Z255(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K8*K6*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z255)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124536',
     & -1.000,V3B,Z255)
       call sum_stripe(6,shape(V3B),size(V3B),'124635',
     & 1.000,V3B,Z255)
       deallocate(Z255)
       deallocate(S160)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S161(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S161)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S161),size(S161),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N1-N1,M1-M1/),'3412',S161,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,M2-N1,M1-N0,M1-M1/),'531246',t3B1,F2)
       allocate(Z256(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K8*K6*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z256)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124536',
     & -1.000,V3B,Z256)
       call sum_stripe(6,shape(V3B),size(V3B),'124635',
     & 1.000,V3B,Z256)
       deallocate(Z256)
       deallocate(S161)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Q28(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q28)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X15),size(X15),'21',0.500,
     & X15,Q28)
       deallocate(Q28)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Q29(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q29)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X16),size(X16),'21',0.500,
     & X16,Q29)
       deallocate(Q29)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S162(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S162)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X33),size(X33),'3412',0.500,
     & X33,S162)
       deallocate(S162)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S163(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S163)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X34),size(X34),'3412',0.500,
     & X34,S163)
       deallocate(S163)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,M2-N1/),'3412',t2A,D2)
       allocate(Q30(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q30)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X17),size(X17),'21',-0.500,
     & X17,Q30)
       deallocate(Q30)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,M2-N1/),'3412',t2A,D2)
       allocate(Q31(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q31)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X18),size(X18),'21',-0.500,
     & X18,Q31)
       deallocate(Q31)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(Q32(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q32)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N1+1:M2,N1+1:M2))
       call reorder_shift(2,shape(Q32),size(Q32),shape(B1),size(B1),
     & (/N1-N1,N1-N1/),'21',Q32,B1)
       allocate(F2(N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N1-N1,M2-N2,M2-N1,M1-N0,M1-N0,M1-M1/),'312456',t3B1,F2)
       allocate(Z263(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K8*K6*K6
       I3=K9
       call EGEMM(I1,I2,I3,B1,F2,Z263)
       deallocate(B1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124563',
     & 0.500,V3B,Z263)
       deallocate(Z263)
       deallocate(Q32)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-M2,M2-M2,N1-N1,M1-M1/),'461235',t3C3,F2)
       allocate(S164(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S164)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S164)
       deallocate(S164)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-M2,M2-M2,N1-N1,M1-M1/),'461235',t3C3,F2)
       allocate(S165(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S165)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S165)
       deallocate(S165)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-M2,M2-M2,N1-N1,M1-M1/),'461235',t3C3,F2)
       allocate(S166(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S166)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S166)
       deallocate(S166)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-M2,M2-M2,N1-N1,M1-M1/),'461235',t3C3,F2)
       allocate(S167(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S167)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S167)
       deallocate(S167)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,M2-N2,N1-N1,M1-M1/),'462135',t3C1,F2)
       allocate(S168(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S168)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',1.000,X3,
     & S168)
       deallocate(S168)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N2-N2,M2-N2,N1-N1,M1-M1/),'462135',t3C1,F2)
       allocate(S169(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S169)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',1.000,X3,
     & S169)
       deallocate(S169)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,M2-N2,N1-N1,M1-M1/),'462135',t3C1,F2)
       allocate(S170(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S170)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',1.000,X3,
     & S170)
       deallocate(S170)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,M2-N2,N1-N1,M1-M1/),'462135',t3C1,F2)
       allocate(S171(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S171)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',1.000,X3,
     & S171)
       deallocate(S171)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,M2-N2,M2-N1,M1-M1/),'462135',t3C1,F2)
       allocate(S172(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S172)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S172)
       deallocate(S172)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N2-N2,M2-N2,M2-N1,M1-M1/),'462135',t3C1,F2)
       allocate(S173(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S173)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S173)
       deallocate(S173)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,M2-N2,M2-N1,M1-M1/),'462135',t3C1,F2)
       allocate(S174(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S174)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S174)
       deallocate(S174)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,M2-N2,M2-N1,M1-M1/),'462135',t3C1,F2)
       allocate(S175(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S175)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S175)
       deallocate(S175)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,M2-N2,M1-M1,M1-N0/),'423156',t3C1,F2)
       allocate(S176(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S176)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X82),size(X82),'2341',-1.000,
     & X82,S176)
       deallocate(S176)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,M2-N2,M1-M1,M1-N0/),'423156',t3C1,F2)
       allocate(S177(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S177)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X82),size(X82),'2341',-1.000,
     & X82,S177)
       deallocate(S177)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,N1-N1,M2-M2,M1-M1,M1-N0/),'413256',t3C3,F2)
       allocate(S178(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S178)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X82),size(X82),'2341',1.000,
     & X82,S178)
       deallocate(S178)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,N1-N1,M2-M2,M1-M1,M1-N0/),'413256',t3C3,F2)
       allocate(S179(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S179)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X82),size(X82),'2341',1.000,
     & X82,S179)
       deallocate(S179)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,M2-N2,M1-M1,M1-N0/),'423156',t3C1,F2)
       allocate(S180(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S180)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X82),size(X82),'2341',-1.000,
     & X82,S180)
       deallocate(S180)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,M2-N2,M1-M1,M1-N0/),'423156',t3C1,F2)
       allocate(S181(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S181)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X82),size(X82),'2341',-1.000,
     & X82,S181)
       deallocate(S181)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S182(M2+1:N3,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S182)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X83(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       X83=0.0d0
       call sum_stripe(4,shape(X83),size(X83),'3412',1.000,
     & X83,S182)
       deallocate(S182)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S183(M2+1:N3,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S183)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X84(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       X84=0.0d0
       call sum_stripe(4,shape(X84),size(X84),'3412',1.000,
     & X84,S183)
       deallocate(S183)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S184(M2+1:N3,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S184)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X85(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       X85=0.0d0
       call sum_stripe(4,shape(X85),size(X85),'3412',1.000,
     & X85,S184)
       deallocate(S184)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S185(M2+1:N3,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S185)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X86(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       X86=0.0d0
       call sum_stripe(4,shape(X86),size(X86),'3412',1.000,
     & X86,S185)
       deallocate(S185)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S186(N1+1:M2,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S186)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X87(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       X87=0.0d0
       call sum_stripe(4,shape(X87),size(X87),'3412',1.000,
     & X87,S186)
       deallocate(S186)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S187(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S187)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X88(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       X88=0.0d0
       call sum_stripe(4,shape(X88),size(X88),'3412',1.000,
     & X88,S187)
       deallocate(S187)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,M2-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(S188(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K6
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S188)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S188)
       deallocate(S188)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(S189(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S189)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S189)
       deallocate(S189)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'3412',VBHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S190(M2+1:N3,N1+1:M2,N2+1:N3,M1+1:N1))
       I1=K7*K4
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S190)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2314',1.000,
     & X10,S190)
       deallocate(S190)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'3412',VBHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1/),'3412',t2B,D2)
       allocate(S191(M2+1:N3,M2+1:N3,N2+1:N3,M1+1:N1))
       I1=K7*K4
       I2=K6*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S191)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2314',1.000,X9,
     & S191)
       deallocate(S191)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'4132',VBHHHP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S192(N1+1:M2,M1+1:N2,N0+1:N2,M1+1:N1))
       I1=K7*K2
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S192)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2314',-1.000,
     & X8,S192)
       deallocate(S192)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'4132',VBHHHP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'4123',t2B,D2)
       allocate(S193(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N1))
       I1=K7*K2
       I2=K8*K6
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S193)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2314',-1.000,
     & X7,S193)
       deallocate(S193)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S194(M2+1:N3,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S194)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X63),size(X63),'2314',1.000,
     & X63,S194)
       deallocate(S194)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S195(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S195)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X64),size(X64),'2314',1.000,
     & X64,S195)
       deallocate(S195)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S196(N1+1:M2,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S196)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X89(N1+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       X89=0.0d0
       call sum_stripe(4,shape(X89),size(X89),'3412',1.000,
     & X89,S196)
       deallocate(S196)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S197(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S197)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X89),size(X89),'2413',-1.000,
     & X89,S197)
       deallocate(S197)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,M2-N1/),'3214',VBPHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S198(M2+1:N3,M1+1:N1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S198)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X90(N2+1:N3,M2+1:N3,M2+1:N3,M1+1:N1))
       X90=0.0d0
       call sum_stripe(4,shape(X90),size(X90),'2413',1.000,
     & X90,S198)
       deallocate(S198)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,M2-N1/),'1234',VBPHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(S199(M1+1:N2,M1+1:N1,N0+1:N2,M2+1:N3))
       I1=K6*K2
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S199)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'3412',1.000,X7,
     & S199)
       deallocate(S199)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N1-N1/),'3214',VBPHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S200(M2+1:N3,M1+1:N1,N2+1:N3,N1+1:M2))
       I1=K9*K4
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S200)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X91(N2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       X91=0.0d0
       call sum_stripe(4,shape(X91),size(X91),'2413',1.000,
     & X91,S200)
       deallocate(S200)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,N1-N1/),'1234',VBPHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(S201(M1+1:N2,M1+1:N1,N0+1:N2,N1+1:M2))
       I1=K9*K2
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S201)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'3412',1.000,X8,
     & S201)
       deallocate(S201)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'4132',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S202(N1+1:M2,M1+1:N1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S202)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X92(N0+1:N2,N1+1:M2,M1+1:N2,M1+1:N1))
       X92=0.0d0
       call sum_stripe(4,shape(X92),size(X92),'2413',1.000,
     & X92,S202)
       deallocate(S202)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S203(M2+1:N3,M1+1:N1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S203)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X93(N0+1:N2,M2+1:N3,M1+1:N2,M1+1:N1))
       X93=0.0d0
       call sum_stripe(4,shape(X93),size(X93),'2413',1.000,
     & X93,S203)
       deallocate(S203)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M2-N2/),'4213',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S204(N1+1:M2,M1+1:N1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S204)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X91),size(X91),'3412',-1.000,
     & X91,S204)
       deallocate(S204)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S205(M2+1:N3,M1+1:N1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S205)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X90),size(X90),'3412',1.000,
     & X90,S205)
       deallocate(S205)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S206(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S206)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X89),size(X89),'2341',0.500,
     & X89,S206)
       deallocate(S206)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S207(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S207)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X89),size(X89),'2341',1.000,
     & X89,S207)
       deallocate(S207)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S208(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S208)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X89),size(X89),'2341',0.500,
     & X89,S208)
       deallocate(S208)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,M2-N1,N1-N1,M1-M1/),'452136',t3A,F2)
       allocate(S209(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S209)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X89),size(X89),'2341',-0.500,
     & X89,S209)
       deallocate(S209)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,M2-N1,N1-N1,M1-M1/),'452136',t3A,F2)
       allocate(S210(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S210)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X89),size(X89),'2341',-1.000,
     & X89,S210)
       deallocate(S210)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,M2-N1,N1-N1,M1-M1/),'452136',t3A,F2)
       allocate(S211(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S211)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X89),size(X89),'2341',-0.500,
     & X89,S211)
       deallocate(S211)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,M2-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(S212(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S212)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',0.500,X5,
     & S212)
       deallocate(S212)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,M2-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(S213(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S213)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',0.500,X5,
     & S213)
       deallocate(S213)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(S214(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S214)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',1.000,X5,
     & S214)
       deallocate(S214)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(S215(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S215)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',1.000,X5,
     & S215)
       deallocate(S215)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(S216(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S216)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',0.500,X5,
     & S216)
       deallocate(S216)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(S217(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S217)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',0.500,X5,
     & S217)
       deallocate(S217)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N1-N1,M2-N1,M1-N0,M1-M1/),'413256',t3A,F2)
       allocate(S218(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S218)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S218)
       deallocate(S218)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N1-N1,M2-N1,M1-N0,M1-M1/),'413256',t3A,F2)
       allocate(S219(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S219)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S219)
       deallocate(S219)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N1-N1,M2-N1,M1-N0,M1-M1/),'423156',t3A,F2)
       allocate(S220(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S220)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-0.500,
     & X4,S220)
       deallocate(S220)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N1-N1,M2-N1,M1-N0,M1-M1/),'423156',t3A,F2)
       allocate(S221(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S221)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-0.500,
     & X4,S221)
       deallocate(S221)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S222(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S222)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'3412',1.000,
     & X11,S222)
       deallocate(S222)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S223(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S223)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'3412',1.000,
     & X12,S223)
       deallocate(S223)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S224(M2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S224)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'3412',1.000,
     & X13,S224)
       deallocate(S224)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S225(M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S225)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'3412',1.000,
     & X14,S225)
       deallocate(S225)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S226(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S226)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X89),size(X89),'2341',-1.000,
     & X89,S226)
       deallocate(S226)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S227(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S227)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X89),size(X89),'2341',-1.000,
     & X89,S227)
       deallocate(S227)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S228(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S228)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X89),size(X89),'2341',-1.000,
     & X89,S228)
       deallocate(S228)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S229(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S229)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X89),size(X89),'2341',-1.000,
     & X89,S229)
       deallocate(S229)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S230(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S230)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X89),size(X89),'2341',-1.000,
     & X89,S230)
       deallocate(S230)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S231(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S231)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X89),size(X89),'2341',-1.000,
     & X89,S231)
       deallocate(S231)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S232(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S232)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X89),size(X89),'2341',-1.000,
     & X89,S232)
       deallocate(S232)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S233(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S233)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X89),size(X89),'2341',-1.000,
     & X89,S233)
       deallocate(S233)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,M2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S234(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S234)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X91),size(X91),'2341',1.000,
     & X91,S234)
       deallocate(S234)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N1,M2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S235(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S235)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X91),size(X91),'2341',1.000,
     & X91,S235)
       deallocate(S235)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,M2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S236(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S236)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X91),size(X91),'2341',1.000,
     & X91,S236)
       deallocate(S236)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,M2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S237(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S237)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X91),size(X91),'2341',1.000,
     & X91,S237)
       deallocate(S237)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,M2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S238(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S238)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X91),size(X91),'2341',1.000,
     & X91,S238)
       deallocate(S238)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N1-N1,M2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S239(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S239)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X91),size(X91),'2341',1.000,
     & X91,S239)
       deallocate(S239)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,M2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S240(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S240)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X91),size(X91),'2341',1.000,
     & X91,S240)
       deallocate(S240)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,M2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S241(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S241)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X91),size(X91),'2341',1.000,
     & X91,S241)
       deallocate(S241)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,M2-N2,M2-N1,M1-M1/),'453126',t3B1,F2)
       allocate(S242(M2+1:N3,M2+1:N3,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K6*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S242)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X90),size(X90),'2341',-1.000,
     & X90,S242)
       deallocate(S242)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N1-N1,M2-N2,M2-N1,M1-M1/),'453126',t3B1,F2)
       allocate(S243(M2+1:N3,M2+1:N3,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K6*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S243)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X90),size(X90),'2341',-1.000,
     & X90,S243)
       deallocate(S243)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,M2-N2,M2-N1,M1-M1/),'453126',t3B1,F2)
       allocate(S244(M2+1:N3,M2+1:N3,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K6*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S244)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X90),size(X90),'2341',-1.000,
     & X90,S244)
       deallocate(S244)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,M2-N2,M2-N1,M1-M1/),'453126',t3B1,F2)
       allocate(S245(M2+1:N3,M2+1:N3,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K6*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S245)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X90),size(X90),'2341',-1.000,
     & X90,S245)
       deallocate(S245)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(S246(M1+1:N2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S246)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X35),size(X35),'3412',1.000,
     & X35,S246)
       deallocate(S246)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(S247(M1+1:N2,M1+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S247)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X36),size(X36),'3412',1.000,
     & X36,S247)
       deallocate(S247)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,M1-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(S248(M1+1:N2,M1+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S248)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X37),size(X37),'3412',1.000,
     & X37,S248)
       deallocate(S248)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(S249(M1+1:N2,M1+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S249)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X38),size(X38),'3412',1.000,
     & X38,S249)
       deallocate(S249)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S250(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S250)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X92),size(X92),'2341',-1.000,
     & X92,S250)
       deallocate(S250)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S251(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S251)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X92),size(X92),'2341',-1.000,
     & X92,S251)
       deallocate(S251)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S252(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S252)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X92),size(X92),'2341',-1.000,
     & X92,S252)
       deallocate(S252)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S253(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S253)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X92),size(X92),'2341',-1.000,
     & X92,S253)
       deallocate(S253)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S254(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S254)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X92),size(X92),'2341',-1.000,
     & X92,S254)
       deallocate(S254)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S255(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S255)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X92),size(X92),'2341',-1.000,
     & X92,S255)
       deallocate(S255)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S256(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S256)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X92),size(X92),'2341',-1.000,
     & X92,S256)
       deallocate(S256)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S257(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S257)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X92),size(X92),'2341',-1.000,
     & X92,S257)
       deallocate(S257)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-M2,M2-M2,M1-N0,M1-M1/),'512346',t3B3,F2)
       allocate(S258(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S258)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X93),size(X93),'2341',1.000,
     & X93,S258)
       deallocate(S258)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-M2,M2-M2,M1-N0,M1-M1/),'512346',t3B3,F2)
       allocate(S259(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S259)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X93),size(X93),'2341',1.000,
     & X93,S259)
       deallocate(S259)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,M2-N1,M1-N0,M1-M1/),'513246',t3B1,F2)
       allocate(S260(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S260)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X93),size(X93),'2341',-1.000,
     & X93,S260)
       deallocate(S260)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,M2-N1,M1-N0,M1-M1/),'513246',t3B1,F2)
       allocate(S261(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S261)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X93),size(X93),'2341',-1.000,
     & X93,S261)
       deallocate(S261)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,M2-N1,M1-N0,M1-M1/),'513246',t3B1,F2)
       allocate(S262(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S262)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X93),size(X93),'2341',-1.000,
     & X93,S262)
       deallocate(S262)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,M2-N1,M1-N0,M1-M1/),'513246',t3B1,F2)
       allocate(S263(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K6
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S263)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X93),size(X93),'2341',-1.000,
     & X93,S263)
       deallocate(S263)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S264(M2+1:N3,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S264)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X67),size(X67),'3412',-1.000,
     & X67,S264)
       deallocate(S264)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S265(M2+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S265)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X68),size(X68),'3412',-1.000,
     & X68,S265)
       deallocate(S265)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S266(M2+1:N3,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S266)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X69),size(X69),'3412',-1.000,
     & X69,S266)
       deallocate(S266)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S267(M2+1:N3,M1+1:N1,M1+1:N1,N2+1:M2))
       I1=K0*K7
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S267)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X70),size(X70),'3412',-1.000,
     & X70,S267)
       deallocate(S267)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S268(M2+1:N3,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S268)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X27),size(X27),'3412',-1.000,
     & X27,S268)
       deallocate(S268)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S269(M2+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S269)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X28),size(X28),'3412',-1.000,
     & X28,S269)
       deallocate(S269)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S270(M2+1:N3,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S270)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X29),size(X29),'3412',-1.000,
     & X29,S270)
       deallocate(S270)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S271(M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S271)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X30),size(X30),'3412',-1.000,
     & X30,S271)
       deallocate(S271)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S272(N1+1:M2,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S272)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X31),size(X31),'3412',-1.000,
     & X31,S272)
       deallocate(S272)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,M1,N1,X31,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,M2-N1,M1-N0,M1-M1/),'531246',t3B1,F2)
       allocate(Z31(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K8*K6*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X31,F2,Z31)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124536',
     & 1.000,V3B,Z31)
       call sum_stripe(6,shape(V3B),size(V3B),'124635',
     & -1.000,V3B,Z31)
       deallocate(Z31)
       deallocate(X31)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S273(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S273)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X32),size(X32),'3412',-1.000,
     & X32,S273)
       deallocate(S273)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,M1,N1,X32,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,M2-N1,M1-N0,M1-M1/),'531246',t3B1,F2)
       allocate(Z32(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K8*K6*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X32,F2,Z32)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124536',
     & 1.000,V3B,Z32)
       call sum_stripe(6,shape(V3B),size(V3B),'124635',
     & -1.000,V3B,Z32)
       deallocate(Z32)
       deallocate(X32)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(Q33(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q33)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X15),size(X15),'21',1.000,
     & X15,Q33)
       deallocate(Q33)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(Q34(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q34)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X16),size(X16),'21',1.000,
     & X16,Q34)
       deallocate(Q34)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S274(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S274)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S274)
       deallocate(S274)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S275(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S275)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S275)
       deallocate(S275)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S276(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S276)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S276)
       deallocate(S276)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S277(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S277)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S277)
       deallocate(S277)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S278(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S278)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S278)
       deallocate(S278)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S279(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S279)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S279)
       deallocate(S279)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S280(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S280)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S280)
       deallocate(S280)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S281(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S281)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S281)
       deallocate(S281)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-M2,M2-M2,M1-N0,M1-M1/),'412356',t3B3,F2)
       allocate(S282(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S282)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S282)
       deallocate(S282)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-M2,M2-M2,M1-N0,M1-M1/),'412356',t3B3,F2)
       allocate(S283(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S283)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S283)
       deallocate(S283)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,M2-N1,M1-N0,M1-M1/),'413256',t3B1,F2)
       allocate(S284(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S284)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S284)
       deallocate(S284)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,M2-N1,M1-N0,M1-M1/),'413256',t3B1,F2)
       allocate(S285(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S285)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S285)
       deallocate(S285)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,M2-N1,M1-N0,M1-M1/),'413256',t3B1,F2)
       allocate(S286(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S286)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S286)
       deallocate(S286)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,M2-N1,M1-N0,M1-M1/),'413256',t3B1,F2)
       allocate(S287(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S287)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S287)
       deallocate(S287)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S288(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S288)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S288),size(S288),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,M1-M1/),'3412',S288,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z390(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z390)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'235614',
     & 1.000,V3B,Z390)
       deallocate(Z390)
       deallocate(S288)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S289(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S289)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S289),size(S289),shape(D1),size(D1),
     & (/M1-M1,M2-M2,M2-M2,M1-M1/),'3412',S289,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z391(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z391)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'235614',
     & 1.000,V3B,Z391)
       deallocate(Z391)
       deallocate(S289)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S290(M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S290)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S290),size(S290),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'3412',S290,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z392(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z392)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'235614',
     & 1.000,V3B,Z392)
       deallocate(Z392)
       deallocate(S290)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S291(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S291)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S291),size(S291),shape(D1),size(D1),
     & (/M1-M1,N2-N2,M2-M2,M1-M1/),'3412',S291,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z393(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z393)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'235614',
     & 1.000,V3B,Z393)
       deallocate(Z393)
       deallocate(S291)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'4123',t2B,D2)
       allocate(S292(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S292)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X71),size(X71),'3412',1.000,
     & X71,S292)
       deallocate(S292)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'4123',t2B,D2)
       allocate(S293(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S293)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X72),size(X72),'3412',1.000,
     & X72,S293)
       deallocate(S293)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'4123',t2B,D2)
       allocate(S294(M2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K6
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S294)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X73),size(X73),'3412',1.000,
     & X73,S294)
       deallocate(S294)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'4123',t2B,D2)
       allocate(S295(M2+1:N3,M1+1:N2,M1+1:N2,N1+1:M2))
       I1=K9*K8
       I2=K8*K6
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S295)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X74),size(X74),'3412',1.000,
     & X74,S295)
       deallocate(S295)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S296(N1+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S296)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X75),size(X75),'3412',1.000,
     & X75,S296)
       deallocate(S296)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S297(N1+1:M2,M1+1:N2,M1+1:N2,N1+1:M2))
       I1=K9*K8
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S297)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X76),size(X76),'3412',1.000,
     & X76,S297)
       deallocate(S297)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(Q35(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q35)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X77),size(X77),'21',1.000,
     & X77,Q35)
       deallocate(Q35)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(Q36(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q36)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X78),size(X78),'21',1.000,
     & X78,Q36)
       deallocate(Q36)
C
       allocate(D1(M2+1:N3,M2+1:N3,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M2-N2,M2-N1,N0-N0,N0-N0/),'1234',VBHHPP,D1)
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M2-N2,M2-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3B1,F2)
       allocate(U402(N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K7*K7*K8*K9
       I3=K6*K6
       call EGEMM(I1,I2,I3,D1,F2,U402)
       deallocate(D1)
       deallocate(F2)
C
       allocate(F1(N0+1:N2,N0+1:N1,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(U402),size(U402),shape(F1),size(F1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1,M1-M1,M1-M1/),'561234',U402,F1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1/),'3412',t2B,D2)
       allocate(Z403(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K8*K9
       I2=K6*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,F1,D2,Z403)
       deallocate(F1)
       deallocate(D2)
C
       V3B=V3B+Z403
       deallocate(Z403)
C
       allocate(F1(N0+1:N1,N0+1:N2,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(U402),size(U402),shape(F1),size(F1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1,M1-M1,M1-M1/),'651234',U402,F1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(U517(M2+1:N3,N0+1:N2,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K8*K9*K2
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,F1,B2,U517)
       deallocate(F1)
       deallocate(B2)
C
       call sum_stripe(6,shape(X81),size(X81),'213456',
     & -1.000,X81,U517)
       deallocate(U517)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Z187(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K8*K9*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,X81,B2,Z187)
       deallocate(B2)
C
       V3B=V3B-Z187
       deallocate(Z187)
       deallocate(X81)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1/),'3412',t2B,D2)
       allocate(S299(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K6*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S299)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X50),size(X50),'3412',1.000,
     & X50,S299)
       deallocate(S299)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1/),'3412',t2B,D2)
       allocate(S300(M2+1:N3,M2+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K6*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S300)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X51),size(X51),'3412',1.000,
     & X51,S300)
       deallocate(S300)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S301(M2+1:N3,N1+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S301)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X52),size(X52),'3412',1.000,
     & X52,S301)
       deallocate(S301)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S302(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S302)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X53),size(X53),'3412',1.000,
     & X53,S302)
       deallocate(S302)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S303(M2+1:N3,N1+1:M2,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S303)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X54),size(X54),'3412',1.000,
     & X54,S303)
       deallocate(S303)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',t2B,D2)
       allocate(Q37(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q37)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X79),size(X79),'21',-1.000,
     & X79,Q37)
       deallocate(Q37)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',t2B,D2)
       allocate(Q38(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q38)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X80),size(X80),'21',-1.000,
     & X80,Q38)
       deallocate(Q38)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',t2B,D2)
       allocate(Q39(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q39)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X17),size(X17),'21',-1.000,
     & X17,Q39)
       deallocate(Q39)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',t2B,D2)
       allocate(Q40(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q40)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X18),size(X18),'21',-1.000,
     & X18,Q40)
       deallocate(Q40)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(Q41(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q41)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X19),size(X19),'21',-1.000,
     & X19,Q41)
       deallocate(Q41)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C2),size(t3C2),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-M2,M2-M2,N1-N1,M1-M1/),'451236',t3C2,F2)
       allocate(S304(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S304)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X91),size(X91),'2341',-0.500,
     & X91,S304)
       deallocate(S304)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-M2,M2-M2,N1-N1,M1-N0/),'451236',t3C3,F2)
       allocate(S305(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S305)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X91),size(X91),'2341',-1.000,
     & X91,S305)
       deallocate(S305)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-M2,M2-M2,N1-N1,M1-N0/),'451236',t3C3,F2)
       allocate(S306(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S306)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X91),size(X91),'2341',-0.500,
     & X91,S306)
       deallocate(S306)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,M2-N2,N1-N1,M1-M1/),'452136',t3C4,F2)
       allocate(S307(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S307)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X91),size(X91),'2341',0.500,
     & X91,S307)
       deallocate(S307)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,M2-N2,N1-N1,M1-N0/),'452136',t3C1,F2)
       allocate(S308(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S308)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X91),size(X91),'2341',1.000,
     & X91,S308)
       deallocate(S308)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,M2-N2,N1-N1,M1-N0/),'452136',t3C1,F2)
       allocate(S309(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S309)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X91),size(X91),'2341',0.500,
     & X91,S309)
       deallocate(S309)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,M2-N2,M2-N1,M1-M1/),'452136',t3C4,F2)
       allocate(S310(M2+1:N3,M2+1:N3,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K6*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S310)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X90),size(X90),'2341',-0.500,
     & X90,S310)
       deallocate(S310)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,M2-N2,M2-N1,M1-N0/),'452136',t3C1,F2)
       allocate(S311(M2+1:N3,M2+1:N3,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K6*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S311)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X90),size(X90),'2341',-1.000,
     & X90,S311)
       deallocate(S311)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,M2-N2,M2-N1,M1-N0/),'452136',t3C1,F2)
       allocate(S312(M2+1:N3,M2+1:N3,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K6*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S312)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X90),size(X90),'2341',-0.500,
     & X90,S312)
       deallocate(S312)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,M2-M2,N1-N1,M1-M1,M1-N0/),'412356',t3C3,F2)
       allocate(S313(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S313)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X92),size(X92),'2341',0.500,
     & X92,S313)
       deallocate(S313)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,M2-M2,N1-N1,M1-M1,M1-N0/),'412356',t3C3,F2)
       allocate(S314(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S314)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X92),size(X92),'2341',0.500,
     & X92,S314)
       deallocate(S314)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N1-N1,M1-M1,M1-N0/),'412356',t3C1,F2)
       allocate(S315(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S315)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X92),size(X92),'2341',1.000,
     & X92,S315)
       deallocate(S315)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N1-N1,M1-M1,M1-N0/),'412356',t3C1,F2)
       allocate(S316(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S316)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X92),size(X92),'2341',1.000,
     & X92,S316)
       deallocate(S316)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N1-N1,M1-M1,M1-N0/),'412356',t3C1,F2)
       allocate(S317(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S317)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X92),size(X92),'2341',0.500,
     & X92,S317)
       deallocate(S317)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N1-N1,M1-M1,M1-N0/),'412356',t3C1,F2)
       allocate(S318(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S318)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X92),size(X92),'2341',0.500,
     & X92,S318)
       deallocate(S318)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,M2-N1,M1-M1,M1-N0/),'412356',t3C1,F2)
       allocate(S319(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K6
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S319)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X93),size(X93),'2341',1.000,
     & X93,S319)
       deallocate(S319)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,M2-N1,M1-M1,M1-N0/),'412356',t3C1,F2)
       allocate(S320(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S320)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X93),size(X93),'2341',1.000,
     & X93,S320)
       deallocate(S320)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,M2-N1,M1-M1,M1-N0/),'412356',t3C1,F2)
       allocate(S321(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K6
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S321)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X93),size(X93),'2341',0.500,
     & X93,S321)
       deallocate(S321)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,M2-N1,M1-M1,M1-N0/),'412356',t3C1,F2)
       allocate(S322(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S322)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X93),size(X93),'2341',0.500,
     & X93,S322)
       deallocate(S322)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S323(M2+1:N3,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S323)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X59(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       X59=0.0d0
       call sum_stripe(4,shape(X59),size(X59),'3412',1.000,
     & X59,S323)
       deallocate(S323)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,M1,N1,X59,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,M2-M2,N1-N1,M1-M1,M1-N0/),'412356',t3C3,F2)
       allocate(Z59(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X59,F2,Z59)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & -1.000,V3B,Z59)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & 1.000,V3B,Z59)
       deallocate(Z59)
       deallocate(X59)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S324(M2+1:N3,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S324)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X60(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       X60=0.0d0
       call sum_stripe(4,shape(X60),size(X60),'3412',1.000,
     & X60,S324)
       deallocate(S324)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,M2,N3,M1,N1,X60,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,M2-M2,N1-N1,M1-M1,M1-N0/),'412356',t3C3,F2)
       allocate(Z60(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X60,F2,Z60)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & -1.000,V3B,Z60)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & 1.000,V3B,Z60)
       deallocate(Z60)
       deallocate(X60)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S325(M2+1:N3,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S325)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X61(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       X61=0.0d0
       call sum_stripe(4,shape(X61),size(X61),'3412',1.000,
     & X61,S325)
       deallocate(S325)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,M2,N3,M1,N1,X61,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N1-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z61(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X61,F2,Z61)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & 1.000,V3B,Z61)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & -1.000,V3B,Z61)
       deallocate(Z61)
       deallocate(X61)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S326(M2+1:N3,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S326)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X62(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       X62=0.0d0
       call sum_stripe(4,shape(X62),size(X62),'3412',1.000,
     & X62,S326)
       deallocate(S326)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,M2,N3,M1,N1,X62,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N1-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z62(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X62,F2,Z62)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & 1.000,V3B,Z62)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & -1.000,V3B,Z62)
       deallocate(Z62)
       deallocate(X62)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S327(N1+1:M2,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S327)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X87),size(X87),'3412',1.000,
     & X87,S327)
       deallocate(S327)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S328(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S328)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X88),size(X88),'3412',1.000,
     & X88,S328)
       deallocate(S328)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S329(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S329)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'3412',1.000,
     & X11,S329)
       deallocate(S329)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S330(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S330)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'3412',1.000,
     & X12,S330)
       deallocate(S330)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S331(M2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S331)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'3412',1.000,
     & X13,S331)
       deallocate(S331)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S332(M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S332)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'3412',1.000,
     & X14,S332)
       deallocate(S332)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S333(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S333)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X55),size(X55),'3412',-1.000,
     & X55,S333)
       deallocate(S333)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S334(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S334)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X56),size(X56),'3412',-1.000,
     & X56,S334)
       deallocate(S334)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S335(M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S335)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X57),size(X57),'3412',-1.000,
     & X57,S335)
       deallocate(S335)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S336(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S336)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X58),size(X58),'3412',-1.000,
     & X58,S336)
       deallocate(S336)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Q42(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q42)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X20),size(X20),'21',0.500,
     & X20,Q42)
       deallocate(Q42)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Q43(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q43)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X21),size(X21),'21',0.500,
     & X21,Q43)
       deallocate(Q43)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,M2-N2/),'3412',t2C,D2)
       allocate(Q44(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q44)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X22),size(X22),'21',-0.500,
     & X22,Q44)
       deallocate(Q44)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,M2-N2/),'3412',t2C,D2)
       allocate(Q45(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q45)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X23),size(X23),'21',-0.500,
     & X23,Q45)
       deallocate(Q45)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M1-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S337(M1+1:N1,N0+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S337)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S337),size(S337),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3214',S337,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S339(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S339)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X64),size(X64),'2134',1.000,
     & X64,S339)
       deallocate(S339)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S337),size(S337),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'2314',S337,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S338(M2+1:N3,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S338)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X63),size(X63),'2134',-1.000,
     & X63,S338)
       deallocate(S338)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S340(N1+1:M2,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S340)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S340),size(S340),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2314',S340,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S341(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S341)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'2134',1.000,X6,
     & S341)
       deallocate(S341)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,M2-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S342(M1+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S342)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S342),size(S342),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'2341',S342,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S346(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S346)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'3124',-1.000,
     & X6,S346)
       deallocate(S346)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S342),size(S342),shape(D1),size(D1),
     & (/N1-N1,N0-N0,M2-M2,M1-M1/),'3241',S342,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S343(M1+1:N1,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S343)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'3124',1.000,X4,
     & S343)
       deallocate(S343)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N1-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S344(M1+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S344)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S344),size(S344),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2341',S344,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S347(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S347)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'2134',1.000,X6,
     & S347)
       deallocate(S347)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S344),size(S344),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N1-N1,M1-M1/),'3241',S344,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S345(M1+1:N1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S345)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'3124',1.000,X5,
     & S345)
       deallocate(S345)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M1-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S348(M1+1:N1,N0+1:N2,N0+1:N1,M1+1:N2))
       I1=K8*K1*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S348)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(S348),size(S348),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3241',S348,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S350(N1+1:M2,N0+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S350)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',-1.000,
     & X8,S350)
       deallocate(S350)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(S348),size(S348),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'2341',S348,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S386(M2+1:N3,N0+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S386)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S386)
       deallocate(S386)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(S348),size(S348),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3241',S348,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S349(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K2
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S349)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',-1.000,
     & X7,S349)
       deallocate(S349)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,M2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S351(M1+1:N1,N0+1:N1,N2+1:N3,M2+1:N3))
       I1=K6*K4*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S351)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S351),size(S351),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'2341',S351,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S352(M2+1:N3,N2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K4
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S352)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'3124',-1.000,
     & X9,S352)
       deallocate(S352)
C
       allocate(D1(N2+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S351),size(S351),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M2-M2,M1-M1/),'3241',S351,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S387(M1+1:N2,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S387)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'3124',1.000,X1,
     & S387)
       deallocate(S387)
C
       allocate(D1(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S351),size(S351),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'2341',S351,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S353(N1+1:M2,N2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K4
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S353)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'3124',-1.000,
     & X10,S353)
       deallocate(S353)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S358(M1+1:N1,M1+1:N1,M1+1:N1,N1+1:N3))
       I1=K3*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S358)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S358),size(S358),shape(D1),size(D1),
     & (/N1-N1,M1-M1,M1-M1,M1-M1/),'4231',S358,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S359(M1+1:N1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S359)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X26),size(X26),'3124',1.000,
     & X26,S359)
       deallocate(S359)
C
       call sumx_sorted3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,M1,N1,M1,N1,X26,VAHHHH, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,M2-N1,N1-N1,M1-N0/),'561234',t3B1,F2)
       allocate(Z26(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K8*K9*K6*K6
       I3=K7*K7
       call EGEMM(I1,I2,I3,X26,F2,Z26)
       deallocate(F2)
C
       V3B=V3B+0.500*Z26
       deallocate(Z26)
       deallocate(X26)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M2-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S360(M1+1:N1,N0+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S360)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S360),size(S360),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,M1-M1/),'2341',S360,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S361(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S361)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X27),size(X27),'3124',1.000,
     & X27,S361)
       deallocate(S361)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,M1,N1,X27,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N2,N1-N1,M1-N0,M1-M1/),'521346',t3B1,F2)
       allocate(Z27(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X27,F2,Z27)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & 1.000,V3B,Z27)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & -1.000,V3B,Z27)
       deallocate(Z27)
       deallocate(X27)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,M2-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S362(M1+1:N1,N0+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S362)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S362),size(S362),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S362,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S363(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S363)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X28),size(X28),'3124',1.000,
     & X28,S363)
       deallocate(S363)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M1,N1,X28,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N2,N1-N1,M1-N0,M1-M1/),'521346',t3B1,F2)
       allocate(Z28(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X28,F2,Z28)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & 1.000,V3B,Z28)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & -1.000,V3B,Z28)
       deallocate(Z28)
       deallocate(X28)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S364(M1+1:N1,N0+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S364)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S364),size(S364),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'2341',S364,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S365(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S365)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X29),size(X29),'3124',1.000,
     & X29,S365)
       deallocate(S365)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,M2,N3,M1,N1,X29,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,N1-N1,M1-N0,M1-M1/),'521346',t3B1,F2)
       allocate(Z29(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X29,F2,Z29)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & 1.000,V3B,Z29)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & -1.000,V3B,Z29)
       deallocate(Z29)
       deallocate(X29)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S366(M1+1:N1,N0+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S366)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S366),size(S366),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'2341',S366,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S367(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S367)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X30),size(X30),'3124',1.000,
     & X30,S367)
       deallocate(S367)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,M1,N1,X30,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,N1-N1,M1-N0,M1-M1/),'521346',t3B1,F2)
       allocate(Z30(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X30,F2,Z30)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & 1.000,V3B,Z30)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & -1.000,V3B,Z30)
       deallocate(Z30)
       deallocate(X30)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S368(M1+1:N1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S368)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S368),size(S368),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'3241',S368,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S369(N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S369)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X65),size(X65),'3124',-1.000,
     & X65,S369)
       deallocate(S369)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,M2-N1,M1-N0,M1-M1/),'531246',t3B1,F2)
       allocate(Z94(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K8*K6*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X65,F2,Z94)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124536',
     & 1.000,V3B,Z94)
       call sum_stripe(6,shape(V3B),size(V3B),'124635',
     & -1.000,V3B,Z94)
       deallocate(Z94)
       deallocate(X65)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S370(M1+1:N1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S370)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S370),size(S370),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'3241',S370,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S371(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S371)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X66),size(X66),'3124',-1.000,
     & X66,S371)
       deallocate(S371)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,M2-N1,M1-N0,M1-M1/),'531246',t3B1,F2)
       allocate(Z95(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K8*K6*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X66,F2,Z95)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124536',
     & 1.000,V3B,Z95)
       call sum_stripe(6,shape(V3B),size(V3B),'124635',
     & -1.000,V3B,Z95)
       deallocate(Z95)
       deallocate(X66)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q46(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q46),size(Q46),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q46,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(Q47(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q47)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X15),size(X15),'21',1.000,
     & X15,Q47)
       deallocate(Q47)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q48(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(Q48),size(Q48),shape(B1),size(B1),
     & (/N1-N1,M1-M1/),'21',Q48,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(Q49(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q49)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X16),size(X16),'21',1.000,
     & X16,Q49)
       deallocate(Q49)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S372(N1+1:M2,N0+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S372)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S372),size(S372),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N1-N1/),'2341',S372,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S373(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S373)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X33),size(X33),'3124',1.000,
     & X33,S373)
       deallocate(S373)
C
       call sumx_sorted1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & M2,N3,N1,M2,M2,N3,N1,M2,X33,VAAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M2-N1,N1-N1,M2-N2,M1-N0,M1-N0,M1-M1/),'231456',t3B1,F2)
       allocate(Z33(M2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K7*K8*K6
       I3=K9*K6
       call EGEMM(I1,I2,I3,X33,F2,Z33)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'145623',
     & 1.000,V3B,Z33)
       deallocate(Z33)
       deallocate(X33)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S374(N1+1:M2,N0+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S374)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S374),size(S374),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N1-N1/),'2341',S374,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S375(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S375)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X34),size(X34),'3124',1.000,
     & X34,S375)
       deallocate(S375)
C
       call sumx_sorted1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,M2,N1,M2,M2,N3,N1,M2,X34,VAAPPP, 1.000)
C
       allocate(F2(N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N1-N1,N1-N1,M2-N2,M1-N0,M1-N0,M1-M1/),'231456',t3B1,F2)
       allocate(Z34(M2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K7*K8*K6
       I3=K9*K9
       call EGEMM(I1,I2,I3,X34,F2,Z34)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'145623',
     & 0.500,V3B,Z34)
       deallocate(Z34)
       deallocate(X34)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q50(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q50)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q51(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q50,B2,Q51)
       deallocate(B2)
C
       call sum_stripe(2,shape(X17),size(X17),'21',-1.000,
     & X17,Q51)
       deallocate(Q51)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q52(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q52)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q54(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q52,B2,Q54)
       deallocate(B2)
C
       call sum_stripe(2,shape(X19),size(X19),'21',-1.000,
     & X19,Q54)
       deallocate(Q54)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q53(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q52,B2,Q53)
       deallocate(B2)
C
       call sum_stripe(2,shape(X18),size(X18),'21',-1.000,
     & X18,Q53)
       deallocate(Q53)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S376(M1+1:N1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S376)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S376),size(S376),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,M1-M1/),'3241',S376,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S377(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S377)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X83),size(X83),'3124',-1.000,
     & X83,S377)
       deallocate(S377)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,M2-M2,N1-N1,M1-M1,M1-N0/),'412356',t3C3,F2)
       allocate(Z282(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X83,F2,Z282)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & -1.000,V3B,Z282)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & 1.000,V3B,Z282)
       deallocate(Z282)
       deallocate(X83)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S378(M1+1:N1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S378)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S378),size(S378),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'3241',S378,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S379(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S379)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X84),size(X84),'3124',-1.000,
     & X84,S379)
       deallocate(S379)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,M2-M2,N1-N1,M1-M1,M1-N0/),'412356',t3C3,F2)
       allocate(Z283(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X84,F2,Z283)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & -1.000,V3B,Z283)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & 1.000,V3B,Z283)
       deallocate(Z283)
       deallocate(X84)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S380(M1+1:N1,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S380)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S380),size(S380),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'3241',S380,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S384(N1+1:M2,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S384)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X87),size(X87),'3124',-1.000,
     & X87,S384)
       deallocate(S384)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,M2-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z286(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K8*K6*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X87,F2,Z286)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124536',
     & -1.000,V3B,Z286)
       call sum_stripe(6,shape(V3B),size(V3B),'124635',
     & 1.000,V3B,Z286)
       deallocate(Z286)
       deallocate(X87)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S380),size(S380),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'3241',S380,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S381(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S381)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X85),size(X85),'3124',-1.000,
     & X85,S381)
       deallocate(S381)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N1-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z284(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X85,F2,Z284)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & 1.000,V3B,Z284)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & -1.000,V3B,Z284)
       deallocate(Z284)
       deallocate(X85)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S382(M1+1:N1,M1+1:N2,N0+1:N1,N2+1:M2))
       I1=K0*K1*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S382)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S382),size(S382),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'3241',S382,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S385(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S385)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X88),size(X88),'3124',-1.000,
     & X88,S385)
       deallocate(S385)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,M2-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z287(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K8*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X88,F2,Z287)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124536',
     & -1.000,V3B,Z287)
       call sum_stripe(6,shape(V3B),size(V3B),'124635',
     & 1.000,V3B,Z287)
       deallocate(Z287)
       deallocate(X88)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S382),size(S382),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'3241',S382,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S383(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S383)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X86),size(X86),'3124',-1.000,
     & X86,S383)
       deallocate(S383)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N1-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z285(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X86,F2,Z285)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134526',
     & 1.000,V3B,Z285)
       call sum_stripe(6,shape(V3B),size(V3B),'134625',
     & -1.000,V3B,Z285)
       deallocate(Z285)
       deallocate(X86)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S354(M1+1:N1,N0+1:M1,N0+1:M1,N1+1:N3))
       I1=K3*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S354)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S354),size(S354),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N0-N0,M1-M1/),'4231',S354,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S355(M1+1:N1,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S355)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X24),size(X24),'3124',1.000,
     & X24,S355)
       deallocate(S355)
C
       call sumx_sorted3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,M1,N0,M1,M1,N1,M1,N1,X24,VAHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'561234',t3B4,F2)
       allocate(Z24(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K8*K9*K6*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,X24,F2,Z24)
       deallocate(F2)
C
       V3B=V3B+0.500*Z24
       deallocate(Z24)
       deallocate(X24)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S356(M1+1:N1,N0+1:M1,M1+1:N1,N1+1:N3))
       I1=K3*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S356)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S356),size(S356),shape(D1),size(D1),
     & (/N1-N1,N0-N0,M1-M1,M1-M1/),'4231',S356,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S357(M1+1:N1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S357)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X25),size(X25),'3124',1.000,
     & X25,S357)
       deallocate(S357)
C
       call sumx_sorted3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,M1,N1,M1,N1,X25,VAHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,M2-N1,N1-N1,M1-N0/),'561234',t3B1,F2)
       allocate(Z25(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K8*K9*K6*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,X25,F2,Z25)
       deallocate(F2)
C
       V3B=V3B+Z25
       deallocate(Z25)
       deallocate(X25)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S388(M2+1:N3,N0+1:N2,N1+1:N3,M1+1:N2))
       I1=K8*K3*K2
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S388)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S388),size(S388),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'2314',S388,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S389(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S389)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',1.000,X2,
     & S389)
       deallocate(S389)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S390(N1+1:M2,N0+1:N2,N1+1:N3,M1+1:N2))
       I1=K8*K3*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S390)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S390),size(S390),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2314',S390,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S391(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S391)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',1.000,X3,
     & S391)
       deallocate(S391)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S392(M1+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S392)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S392),size(S392),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'2341',S392,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S394(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S394)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'3124',-1.000,
     & X3,S394)
       deallocate(S394)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S392),size(S392),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'2341',S392,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S393(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S393)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'3124',-1.000,
     & X2,S393)
       deallocate(S393)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M1-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S395(M1+1:N2,N0+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S395)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(S395),size(S395),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3214',S395,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S397(N1+1:M2,N0+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S397)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',-1.000,
     & X8,S397)
       deallocate(S397)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(S395),size(S395),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'2314',S395,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S487(M2+1:N3,N0+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S487)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S487)
       deallocate(S487)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(S395),size(S395),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3214',S395,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S396(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K2
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S396)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',-1.000,
     & X7,S396)
       deallocate(S396)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S400(N1+1:M2,N0+1:N2,N2+1:N3,M1+1:N1))
       I1=K7*K4*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S400)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S400),size(S400),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'2314',S400,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S401(M2+1:N3,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S401)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',1.000,
     & X10,S401)
       deallocate(S401)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,M2-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S402(M1+1:N1,N0+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S402)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S402),size(S402),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'2341',S402,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S406(M2+1:N3,N2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S406)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',-1.000,
     & X9,S406)
       deallocate(S406)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S402),size(S402),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M2-M2,M1-M1/),'3241',S402,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S403(M1+1:N2,N0+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S403)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'3124',1.000,X7,
     & S403)
       deallocate(S403)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S404(M1+1:N1,N0+1:N2,N2+1:N3,N1+1:M2))
       I1=K9*K4*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S404)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S404),size(S404),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'2341',S404,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S407(M2+1:N3,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S407)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',-1.000,
     & X10,S407)
       deallocate(S407)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S404),size(S404),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N1-N1,M1-M1/),'3241',S404,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S405(M1+1:N2,N0+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S405)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'3124',1.000,X8,
     & S405)
       deallocate(S405)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S408(M1+1:N1,N0+1:M1,N0+1:M1,N2+1:N3))
       I1=K4*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S408)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S408),size(S408),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N0-N0,M1-M1/),'4231',S408,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S409(M1+1:N2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S409)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X35),size(X35),'3124',1.000,
     & X35,S409)
       deallocate(S409)
C
       call sumx_sorted3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,M1,N0,M1,M1,N2,M1,N1,X35,VBHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z35(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K7*K9*K6*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,X35,F2,Z35)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'123546',
     & -1.000,V3B,Z35)
       call sum_stripe(6,shape(V3B),size(V3B),'123645',
     & 1.000,V3B,Z35)
       deallocate(Z35)
       deallocate(X35)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S410(M1+1:N1,M1+1:N2,N0+1:M1,N2+1:N3))
       I1=K4*K5*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S410)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S410),size(S410),shape(D1),size(D1),
     & (/N2-N2,M1-M1,N0-N0,M1-M1/),'4231',S410,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S411(M1+1:N2,M1+1:N2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S411)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X36),size(X36),'3124',1.000,
     & X36,S411)
       deallocate(S411)
C
       call sumx_sorted3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,M1,N2,M1,N1,X36,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z36(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K7*K9*K6*K6
       I3=K5*K8
       call EGEMM(I1,I2,I3,X36,F2,Z36)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'123546',
     & -1.000,V3B,Z36)
       call sum_stripe(6,shape(V3B),size(V3B),'123645',
     & 1.000,V3B,Z36)
       deallocate(Z36)
       deallocate(X36)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S412(M1+1:N1,N0+1:M1,M1+1:N1,N2+1:N3))
       I1=K4*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S412)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S412),size(S412),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M1-M1,M1-M1/),'4231',S412,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S413(M1+1:N2,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S413)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X37),size(X37),'3124',1.000,
     & X37,S413)
       deallocate(S413)
C
       call sumx_sorted3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,M1,N2,M1,N1,X37,VBHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z37(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K7*K9*K6*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,X37,F2,Z37)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'123546',
     & -1.000,V3B,Z37)
       call sum_stripe(6,shape(V3B),size(V3B),'123645',
     & 1.000,V3B,Z37)
       deallocate(Z37)
       deallocate(X37)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S414(M1+1:N1,M1+1:N2,M1+1:N1,N2+1:N3))
       I1=K4*K7*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S414)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S414),size(S414),shape(D1),size(D1),
     & (/N2-N2,M1-M1,M1-M1,M1-M1/),'4231',S414,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S415(M1+1:N2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S415)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X38),size(X38),'3124',1.000,
     & X38,S415)
       deallocate(S415)
C
       call sumx_sorted3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,M1,N2,M1,N1,X38,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(Z38(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K7*K9*K6*K6
       I3=K7*K8
       call EGEMM(I1,I2,I3,X38,F2,Z38)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'123546',
     & -1.000,V3B,Z38)
       call sum_stripe(6,shape(V3B),size(V3B),'123645',
     & 1.000,V3B,Z38)
       deallocate(Z38)
       deallocate(X38)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S416(M1+1:N1,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S416)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S416),size(S416),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,M1-M1/),'2341',S416,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S417(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S417)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X67),size(X67),'3124',-1.000,
     & X67,S417)
       deallocate(S417)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(Z115(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X67,F2,Z115)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'234516',
     & 1.000,V3B,Z115)
       call sum_stripe(6,shape(V3B),size(V3B),'234615',
     & -1.000,V3B,Z115)
       deallocate(Z115)
       deallocate(X67)
C
       allocate(D1(N1+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S418(M1+1:N1,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S418)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S418),size(S418),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S418,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S419(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S419)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X68),size(X68),'3124',-1.000,
     & X68,S419)
       deallocate(S419)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(Z116(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X68,F2,Z116)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'234516',
     & 1.000,V3B,Z116)
       call sum_stripe(6,shape(V3B),size(V3B),'234615',
     & -1.000,V3B,Z116)
       deallocate(Z116)
       deallocate(X68)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S420(M1+1:N1,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S420)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S420),size(S420),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'2341',S420,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S421(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S421)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X69),size(X69),'3124',-1.000,
     & X69,S421)
       deallocate(S421)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(Z117(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X69,F2,Z117)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'234516',
     & 1.000,V3B,Z117)
       call sum_stripe(6,shape(V3B),size(V3B),'234615',
     & -1.000,V3B,Z117)
       deallocate(Z117)
       deallocate(X69)
C
       allocate(D1(N1+1:N3,N0+1:N2,M1+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S422(M1+1:N1,N0+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S422)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S422),size(S422),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'2341',S422,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S423(M2+1:N3,M1+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S423)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X70),size(X70),'3124',-1.000,
     & X70,S423)
       deallocate(S423)
C
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(Z118(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K8*K9*K6
       I3=K0*K7
       call EGEMM(I1,I2,I3,X70,F2,Z118)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'234516',
     & 1.000,V3B,Z118)
       call sum_stripe(6,shape(V3B),size(V3B),'234615',
     & -1.000,V3B,Z118)
       deallocate(Z118)
       deallocate(X70)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q55(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q55),size(Q55),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q55,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(Q56(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q56)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X15),size(X15),'21',1.000,
     & X15,Q56)
       deallocate(Q56)
C
       call sumx_sorted21(N0,N1,N0,N1,
     & N0,M1,M1,N1,X15,FAHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(Z15(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K8*K9*K6*K6
       I3=K5
       call EGEMM(I1,I2,I3,X15,F2,Z15)
       deallocate(F2)
C
       V3B=V3B+Z15
       call sum_stripe(6,shape(V3B),size(V3B),'123465',
     & -1.000,V3B,Z15)
       deallocate(Z15)
       deallocate(X15)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q57(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(Q57),size(Q57),shape(B1),size(B1),
     & (/N1-N1,M1-M1/),'21',Q57,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(Q58(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q58)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X16),size(X16),'21',1.000,
     & X16,Q58)
       deallocate(Q58)
C
       call sumx_sorted21(N0,N1,N0,N1,
     & M1,N1,M1,N1,X16,FAHH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'512346',t3B1,F2)
       allocate(Z16(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K8*K9*K6*K6
       I3=K7
       call EGEMM(I1,I2,I3,X16,F2,Z16)
       deallocate(F2)
C
       V3B=V3B+Z16
       call sum_stripe(6,shape(V3B),size(V3B),'123465',
     & -1.000,V3B,Z16)
       deallocate(Z16)
       deallocate(X16)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S424(M1+1:N2,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S424)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S424),size(S424),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,M1-M1/),'3241',S424,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S425(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S425)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X71),size(X71),'3124',1.000,
     & X71,S425)
       deallocate(S425)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N2,N1-N1,M1-N0,M1-M1/),'421356',t3B1,F2)
       allocate(Z119(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X71,F2,Z119)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'135624',
     & 1.000,V3B,Z119)
       deallocate(Z119)
       deallocate(X71)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S426(M1+1:N2,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S426)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S426),size(S426),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'3241',S426,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S427(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S427)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X72),size(X72),'3124',1.000,
     & X72,S427)
       deallocate(S427)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N2,N1-N1,M1-N0,M1-M1/),'421356',t3B1,F2)
       allocate(Z120(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X72,F2,Z120)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'135624',
     & 1.000,V3B,Z120)
       deallocate(Z120)
       deallocate(X72)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S428(M1+1:N2,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S428)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S428),size(S428),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'3241',S428,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S432(N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S432)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X75),size(X75),'3124',1.000,
     & X75,S432)
       deallocate(S432)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,M2-N1,M1-N0,M1-M1/),'431256',t3B1,F2)
       allocate(Z123(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K7*K7*K6*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X75,F2,Z123)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'125634',
     & 1.000,V3B,Z123)
       deallocate(Z123)
       deallocate(X75)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S428),size(S428),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'3241',S428,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S429(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S429)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X73),size(X73),'3124',1.000,
     & X73,S429)
       deallocate(S429)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,N1-N1,M1-N0,M1-M1/),'421356',t3B1,F2)
       allocate(Z121(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X73,F2,Z121)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'135624',
     & 1.000,V3B,Z121)
       deallocate(Z121)
       deallocate(X73)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S430(M1+1:N2,M1+1:N2,N0+1:N1,N1+1:M2))
       I1=K9*K1*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S430)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S430),size(S430),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'3241',S430,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S433(N1+1:M2,M1+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S433)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X76),size(X76),'3124',1.000,
     & X76,S433)
       deallocate(S433)
C
       allocate(F2(M1+1:N2,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,M2-N1,M1-N0,M1-M1/),'431256',t3B1,F2)
       allocate(Z124(M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K7*K7*K6*K6
       I3=K9*K8
       call EGEMM(I1,I2,I3,X76,F2,Z124)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'125634',
     & 1.000,V3B,Z124)
       deallocate(Z124)
       deallocate(X76)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S430),size(S430),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'3241',S430,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S431(M2+1:N3,M1+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S431)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X74),size(X74),'3124',1.000,
     & X74,S431)
       deallocate(S431)
C
       allocate(F2(M1+1:N2,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,N1-N1,M1-N0,M1-M1/),'421356',t3B1,F2)
       allocate(Z122(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K9*K8
       call EGEMM(I1,I2,I3,X74,F2,Z122)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'135624',
     & 1.000,V3B,Z122)
       deallocate(Z122)
       deallocate(X74)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q59(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q59),size(Q59),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q59,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q60(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q60)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X77),size(X77),'21',1.000,
     & X77,Q60)
       deallocate(Q60)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z125(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K7*K9*K6*K6
       I3=K5
       call EGEMM(I1,I2,I3,X77,F2,Z125)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'123564',
     & -1.000,V3B,Z125)
       deallocate(Z125)
       deallocate(X77)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q61(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(Q61),size(Q61),shape(B1),size(B1),
     & (/N2-N2,M1-M1/),'21',Q61,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q62(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q62)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X78),size(X78),'21',1.000,
     & X78,Q62)
       deallocate(Q62)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z126(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K7*K9*K6*K6
       I3=K8
       call EGEMM(I1,I2,I3,X78,F2,Z126)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'123564',
     & -1.000,V3B,Z126)
       deallocate(Z126)
       deallocate(X78)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1/),'3412',t2B,D2)
       allocate(S298(M2+1:N3,M2+1:N3,N2+1:M2,M2+1:N3))
       I1=K6*K0
       I2=K6*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S298)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X49),size(X49),'3412',1.000,
     & X49,S298)
       deallocate(S298)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S434(M2+1:N3,N0+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K2
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S434)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(S434),size(S434),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M2-M2/),'2341',S434,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S435(M2+1:N3,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S435)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X49),size(X49),'3124',1.000,
     & X49,S435)
       deallocate(S435)
C
       call sumx_sorted3412(N2,N3,N1,N3,N2,M2,N1,N3,
     & N2,M2,M2,N3,M2,N3,M2,N3,X49,VBPAPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N2-N2,M2-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3B1,F2)
       allocate(Z49(N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K7*K7*K8*K9
       I3=K6*K0
       call EGEMM(I1,I2,I3,X49,F2,Z49)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'345612',
     & 1.000,V3B,Z49)
       deallocate(Z49)
       deallocate(X49)
C
       allocate(D1(N0+1:N1,N0+1:N2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S436(M2+1:N3,N0+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K2
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S436)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(S436),size(S436),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,M2-M2/),'2341',S436,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S437(M2+1:N3,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S437)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X50),size(X50),'3124',1.000,
     & X50,S437)
       deallocate(S437)
C
       call sumx_sorted3412(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,N1,M2,M2,N3,M2,N3,X50,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M2-N2,N1-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3B1,F2)
       allocate(Z50(N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K7*K7*K8*K9
       I3=K9*K6
       call EGEMM(I1,I2,I3,X50,F2,Z50)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'345612',
     & 1.000,V3B,Z50)
       deallocate(Z50)
       deallocate(X50)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S438(M2+1:N3,N0+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K2
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S438)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(S438),size(S438),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M2-M2/),'2341',S438,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S439(M2+1:N3,N2+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S439)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X51),size(X51),'3124',1.000,
     & X51,S439)
       deallocate(S439)
C
       call sumx_sorted3412(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,N1,M2,M2,N3,M2,N3,X51,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N2-N2,N1-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3B1,F2)
       allocate(Z51(N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K7*K7*K8*K9
       I3=K9*K0
       call EGEMM(I1,I2,I3,X51,F2,Z51)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'345612',
     & 1.000,V3B,Z51)
       deallocate(Z51)
       deallocate(X51)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S440(N1+1:M2,N0+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S440)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S440),size(S440),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,N1-N1/),'2341',S440,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S441(M2+1:N3,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S441)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X52),size(X52),'3124',1.000,
     & X52,S441)
       deallocate(S441)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,M2,N3,M2,N3,N1,M2,X52,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/N2-N2,M2-M2,M2-M2,M1-N0,M1-N0,M1-M1/),'123456',t3B3,F2)
       allocate(Z52(M2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K7*K8*K6
       I3=K6*K0
       call EGEMM(I1,I2,I3,X52,F2,Z52)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'245613',
     & -1.000,V3B,Z52)
       deallocate(Z52)
       deallocate(X52)
C
       allocate(D1(N0+1:N1,N0+1:N2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S442(N1+1:M2,N0+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S442)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S442),size(S442),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N1-N1/),'2341',S442,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S443(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S443)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X53),size(X53),'3124',1.000,
     & X53,S443)
       deallocate(S443)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,N1,M2,M2,N3,N1,M2,X53,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M2-N2,N1-N1,M2-N1,M1-N0,M1-N0,M1-M1/),'132456',t3B1,F2)
       allocate(Z53(M2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K7*K8*K6
       I3=K9*K6
       call EGEMM(I1,I2,I3,X53,F2,Z53)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'245613',
     & 1.000,V3B,Z53)
       deallocate(Z53)
       deallocate(X53)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S444(N1+1:M2,N0+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S444)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S444),size(S444),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N1-N1/),'2341',S444,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S445(M2+1:N3,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S445)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X54),size(X54),'3124',1.000,
     & X54,S445)
       deallocate(S445)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,N1,M2,M2,N3,N1,M2,X54,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N2-N2,N1-N1,M2-N1,M1-N0,M1-N0,M1-M1/),'132456',t3B1,F2)
       allocate(Z54(M2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K7*K8*K6
       I3=K9*K0
       call EGEMM(I1,I2,I3,X54,F2,Z54)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'245613',
     & 1.000,V3B,Z54)
       deallocate(Z54)
       deallocate(X54)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q63(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q63)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q64(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q63,B2,Q64)
       deallocate(B2)
C
       call sum_stripe(2,shape(X79),size(X79),'21',-1.000,
     & X79,Q64)
       deallocate(Q64)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M2-N2,M2-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3B1,F2)
       allocate(Z135(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K7*K8*K9*K6
       I3=K6
       call EGEMM(I1,I2,I3,X79,F2,Z135)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'234561',
     & 1.000,V3B,Z135)
       deallocate(Z135)
       deallocate(X79)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q65(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q66(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q65,B2,Q66)
       deallocate(B2)
C
       call sum_stripe(2,shape(X80),size(X80),'21',-1.000,
     & X80,Q66)
       deallocate(Q66)
C
       allocate(F2(N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N2-N2,M2-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3B1,F2)
       allocate(Z136(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K7*K8*K9*K6
       I3=K0
       call EGEMM(I1,I2,I3,X80,F2,Z136)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'234561',
     & 1.000,V3B,Z136)
       deallocate(Z136)
       deallocate(X80)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q67(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q68(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q67,B2,Q68)
       deallocate(B2)
C
       call sum_stripe(2,shape(X17),size(X17),'21',-1.000,
     & X17,Q68)
       deallocate(Q68)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & M2,N3,M2,N3,X17,FAPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M2-N1,M2-N2,N1-N1,M1-N0,M1-N0,M1-M1/),'213456',t3B1,F2)
       allocate(Z17(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K7*K8*K9*K6
       I3=K6
       call EGEMM(I1,I2,I3,X17,F2,Z17)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134562',
     & 1.000,V3B,Z17)
       deallocate(Z17)
       deallocate(X17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q69(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q69)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q71(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q69,B2,Q71)
       deallocate(B2)
C
       call sum_stripe(2,shape(X19),size(X19),'21',-1.000,
     & X19,Q71)
       deallocate(Q71)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & N1,M2,N1,M2,X19,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N1-N1,M2-N2,M2-N1,M1-N0,M1-N0,M1-M1/),'312456',t3B1,F2)
       allocate(Z19(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K8*K6*K6
       I3=K9
       call EGEMM(I1,I2,I3,X19,F2,Z19)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124563',
     & 1.000,V3B,Z19)
       deallocate(Z19)
       deallocate(X19)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q70(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q69,B2,Q70)
       deallocate(B2)
C
       call sum_stripe(2,shape(X18),size(X18),'21',-1.000,
     & X18,Q70)
       deallocate(Q70)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & N1,M2,M2,N3,X18,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N1-N1,M2-N2,N1-N1,M1-N0,M1-N0,M1-M1/),'213456',t3B1,F2)
       allocate(Z18(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K7*K8*K9*K6
       I3=K9
       call EGEMM(I1,I2,I3,X18,F2,Z18)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134562',
     & 1.000,V3B,Z18)
       deallocate(Z18)
       deallocate(X18)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S446(M1+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S446)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S446),size(S446),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'2431',S446,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S448(M2+1:N3,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S448)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X63),size(X63),'2314',1.000,
     & X63,S448)
       deallocate(S448)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S446),size(S446),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'3421',S446,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S449(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S449)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X64),size(X64),'2314',-1.000,
     & X64,S449)
       deallocate(S449)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S446),size(S446),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'2341',S446,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S450(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S450)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2314',0.500,X6,
     & S450)
       deallocate(S450)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S446),size(S446),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N0-N0,M1-M1/),'4231',S446,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S535(M1+1:N1,N0+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S535)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S535),size(S535),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'2314',S535,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S536(M2+1:N3,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S536)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',-1.000,
     & X4,S536)
       deallocate(S536)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S446),size(S446),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'3241',S446,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S538(N1+1:M2,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S538)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S538),size(S538),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2314',S538,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S539(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S539)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'2134',1.000,X6,
     & S539)
       deallocate(S539)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S446),size(S446),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'2431',S446,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S447(M2+1:N3,M1+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S447)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S447)
       deallocate(S447)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S535),size(S535),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3214',S535,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S537(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S537)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',1.000,X5,
     & S537)
       deallocate(S537)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q72(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q72)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q72),size(Q72),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q72,B1)
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S458(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S458)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S458)
       deallocate(S458)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q72),size(Q72),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q72,B1)
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,M2-N2,M1-N0,M1-N0/),'2134',t2B,D2)
       allocate(S463(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S463)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X82),size(X82),'2341',1.000,
     & X82,S463)
       deallocate(S463)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q72),size(Q72),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q72,B1)
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,M2-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S456(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S456)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S456)
       deallocate(S456)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S459(N1+1:M2,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S459)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S459),size(S459),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'3412',S459,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S460(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S460)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'2134',-1.000,
     & X6,S460)
       deallocate(S460)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S461(N1+1:M2,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S461)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S461),size(S461),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N1-N1/),'2431',S461,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S462(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S462)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2413',-1.000,
     & X6,S462)
       deallocate(S462)
C
       call sumx_sorted2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M2,N3,N1,M2,M1,N1,X6,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,M2-N2,M1-N0,M1-N0/),'2134',t2B,D2)
       allocate(Z6(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K6
       I2=K7*K8*K6
       I3=K3
       call EGEMM(I1,I2,I3,X6,D2,Z6)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'145236',
     & 1.000,V3B,Z6)
       call sum_stripe(6,shape(V3B),size(V3B),'146235',
     & -1.000,V3B,Z6)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S453(M1+1:N1,M1+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S453)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S453),size(S453),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3412',S453,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S454(M2+1:N3,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S454)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',-0.500,
     & X4,S454)
       deallocate(S454)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S453),size(S453),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'4312',S453,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S457(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S457)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',0.500,X5,
     & S457)
       deallocate(S457)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S466(M2+1:N3,M1+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S466)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S466),size(S466),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'3412',S466,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S468(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S468)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'3124',-1.000,
     & X3,S468)
       deallocate(S468)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S466),size(S466),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'3412',S466,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S467(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S467)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'3124',-1.000,
     & X2,S467)
       deallocate(S467)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S464(M1+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S464)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S464),size(S464),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'2341',S464,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1/),'3412',t2B,D2)
       allocate(S470(M2+1:N3,M2+1:N3,N2+1:N3,M1+1:N1))
       I1=K7*K4
       I2=K6*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S470)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2314',1.000,X9,
     & S470)
       deallocate(S470)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,M1+1:N1))
       call reorder_shift(4,shape(S464),size(S464),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'3421',S464,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S471(N1+1:M2,M1+1:N2,N0+1:N2,M1+1:N1))
       I1=K7*K2
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S471)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2314',-1.000,
     & X8,S471)
       deallocate(S471)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,M1+1:N1))
       call reorder_shift(4,shape(S464),size(S464),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'3421',S464,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'4123',t2B,D2)
       allocate(S472(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N1))
       I1=K7*K2
       I2=K8*K6
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S472)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2314',-1.000,
     & X7,S472)
       deallocate(S472)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S464),size(S464),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S464,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S473(M2+1:N3,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S473)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X63),size(X63),'2314',1.000,
     & X63,S473)
       deallocate(S473)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(Z70(M2+1:N3,N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N1,M1+1:N1))
       I1=K7*K7*K6
       I2=K8*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X63,D2,Z70)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134256',
     & -1.000,V3B,Z70)
       call sum_stripe(6,shape(V3B),size(V3B),'134265',
     & 1.000,V3B,Z70)
       deallocate(Z70)
       deallocate(X63)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S464),size(S464),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S464,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S474(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S474)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X64),size(X64),'2314',1.000,
     & X64,S474)
       deallocate(S474)
C
       allocate(D2(N0+1:N1,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,M2-N2,M2-N1,M1-N0/),'4123',t2B,D2)
       allocate(Z71(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K9
       I2=K8*K6*K6
       I3=K1
       call EGEMM(I1,I2,I3,X64,D2,Z71)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124356',
     & 1.000,V3B,Z71)
       call sum_stripe(6,shape(V3B),size(V3B),'124365',
     & -1.000,V3B,Z71)
       deallocate(Z71)
       deallocate(X64)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S464),size(S464),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S464,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S465(M2+1:N3,M1+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S465)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S465)
       deallocate(S465)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S464),size(S464),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N0-N0,M1-M1/),'4231',S464,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S540(M1+1:N2,N0+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S540)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(S540),size(S540),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3214',S540,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S541(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K2
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S541)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',-1.000,
     & X7,S541)
       deallocate(S541)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S464),size(S464),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'3241',S464,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S545(N1+1:M2,N0+1:N2,N2+1:N3,M1+1:N1))
       I1=K7*K4*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S545)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S545),size(S545),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'2314',S545,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S546(M2+1:N3,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S546)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',1.000,
     & X10,S546)
       deallocate(S546)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S464),size(S464),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'3241',S464,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S543(M2+1:N3,N0+1:N2,N2+1:N3,M1+1:N1))
       I1=K7*K4*K2
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S543)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S543),size(S543),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'2314',S543,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S544(M2+1:N3,N2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S544)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',1.000,X9,
     & S544)
       deallocate(S544)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S464),size(S464),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'2341',S464,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S469(M2+1:N3,N1+1:M2,N2+1:N3,M1+1:N1))
       I1=K7*K4
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S469)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2314',1.000,
     & X10,S469)
       deallocate(S469)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(S540),size(S540),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3214',S540,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S542(N1+1:M2,N0+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S542)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',-1.000,
     & X8,S542)
       deallocate(S542)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(S540),size(S540),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'2314',S540,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S547(M2+1:N3,N0+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S547)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S547)
       deallocate(S547)
C
       call sumx_sorted2134(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M2,N3,M1,N2,M1,N1,X1,VBHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z1(M2+1:N3,N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7*K8*K6
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X1,D2,Z1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'235146',
     & 1.000,V3B,Z1)
       call sum_stripe(6,shape(V3B),size(V3B),'236145',
     & -1.000,V3B,Z1)
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S477(N1+1:M2,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S477)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S477),size(S477),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N1-N1/),'2341',S477,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S478(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S478)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X89),size(X89),'2413',1.000,
     & X89,S478)
       deallocate(S478)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S477),size(S477),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N1-N1/),'2431',S477,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S483(M2+1:N3,M1+1:N1,N2+1:N3,N1+1:M2))
       I1=K9*K4
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S483)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X91),size(X91),'2413',-1.000,
     & X91,S483)
       deallocate(S483)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S477),size(S477),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N1-N1,N1-N1/),'3241',S477,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S550(M1+1:N2,N0+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S550)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S550),size(S550),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2341',S550,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S551(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S551)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',1.000,X3,
     & S551)
       deallocate(S551)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:M2))
       call reorder_shift(4,shape(S477),size(S477),shape(D1),size(D1),
     & (/N2-N2,N1-N1,N0-N0,N1-N1/),'3421',S477,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(S485(M1+1:N2,M1+1:N1,N0+1:N2,N1+1:M2))
       I1=K9*K2
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S485)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'3412',-1.000,
     & X8,S485)
       deallocate(S485)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S490(M1+1:N2,N0+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S490)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S490),size(S490),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2341',S490,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S491(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S491)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',-1.000,
     & X3,S491)
       deallocate(S491)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S492(M1+1:N2,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S492)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S492),size(S492),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,M1-M1/),'2341',S492,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S493(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S493)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'3124',-1.000,
     & X11,S493)
       deallocate(S493)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,M1,N2,X11,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(Z11(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X11,F2,Z11)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'235614',
     & 1.000,V3B,Z11)
       deallocate(Z11)
       deallocate(X11)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S494(M1+1:N2,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S494)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S494),size(S494),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S494,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S495(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S495)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'3124',-1.000,
     & X12,S495)
       deallocate(S495)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M1,N2,X12,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3A,F2)
       allocate(Z12(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X12,F2,Z12)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'235614',
     & 1.000,V3B,Z12)
       deallocate(Z12)
       deallocate(X12)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S496(M1+1:N2,N0+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S496)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S496),size(S496),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'2341',S496,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S497(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S497)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X13),size(X13),'3124',-1.000,
     & X13,S497)
       deallocate(S497)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,M2,N3,M1,N2,X13,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N1,N1-N1,M1-N0,M1-M1/),'421356',t3A,F2)
       allocate(Z13(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X13,F2,Z13)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'235614',
     & -1.000,V3B,Z13)
       deallocate(Z13)
       deallocate(X13)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S498(M1+1:N2,N0+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S498)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S498),size(S498),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'2341',S498,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S499(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S499)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X14),size(X14),'3124',-1.000,
     & X14,S499)
       deallocate(S499)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,M1,N2,X14,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N1,N1-N1,M1-N0,M1-M1/),'421356',t3A,F2)
       allocate(Z14(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X14,F2,Z14)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'235614',
     & -1.000,V3B,Z14)
       deallocate(Z14)
       deallocate(X14)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S500(M1+1:N2,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S500)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S500),size(S500),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,M1-M1/),'2341',S500,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S501(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S501)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X55),size(X55),'3124',1.000,
     & X55,S501)
       deallocate(S501)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,M2,N3,M1,N2,X55,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z55(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X55,F2,Z55)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'235614',
     & -1.000,V3B,Z55)
       deallocate(Z55)
       deallocate(X55)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S502(M1+1:N2,N0+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S502)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S502),size(S502),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S502,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S503(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S503)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X56),size(X56),'3124',1.000,
     & X56,S503)
       deallocate(S503)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,M2,N3,M1,N2,X56,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z56(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X56,F2,Z56)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'235614',
     & -1.000,V3B,Z56)
       deallocate(Z56)
       deallocate(X56)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S504(M1+1:N2,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S504)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S504),size(S504),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'2341',S504,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S505(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S505)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X57),size(X57),'3124',1.000,
     & X57,S505)
       deallocate(S505)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,M2,N3,M1,N2,X57,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z57(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X57,F2,Z57)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'235614',
     & -1.000,V3B,Z57)
       deallocate(Z57)
       deallocate(X57)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S506(M1+1:N2,N0+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S506)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S506),size(S506),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'2341',S506,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S507(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S507)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X58),size(X58),'3124',1.000,
     & X58,S507)
       deallocate(S507)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,M2,N3,M1,N2,X58,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z58(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K7*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X58,F2,Z58)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'235614',
     & -1.000,V3B,Z58)
       deallocate(Z58)
       deallocate(X58)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q74(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q74)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q74),size(Q74),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q74,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q75(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q75)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X20),size(X20),'21',1.000,
     & X20,Q75)
       deallocate(Q75)
C
       call sumx_sorted21(N0,N2,N0,N2,
     & N0,M1,M1,N2,X20,FBHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z20(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K7*K9*K6*K6
       I3=K5
       call EGEMM(I1,I2,I3,X20,F2,Z20)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'123564',
     & -1.000,V3B,Z20)
       deallocate(Z20)
       deallocate(X20)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q76(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q76)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(Q76),size(Q76),shape(B1),size(B1),
     & (/N2-N2,M1-M1/),'21',Q76,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q77(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q77)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X21),size(X21),'21',1.000,
     & X21,Q77)
       deallocate(Q77)
C
       call sumx_sorted21(N0,N2,N0,N2,
     & M1,N2,M1,N2,X21,FBHH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,M1-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z21(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K7*K9*K6*K6
       I3=K8
       call EGEMM(I1,I2,I3,X21,F2,Z21)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'123564',
     & -1.000,V3B,Z21)
       deallocate(Z21)
       deallocate(X21)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q78(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q78)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q79(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q78,B2,Q79)
       deallocate(B2)
C
       call sum_stripe(2,shape(X22),size(X22),'21',-1.000,
     & X22,Q79)
       deallocate(Q79)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & M2,N3,M2,N3,X22,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M2-N2,M2-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3B1,F2)
       allocate(Z22(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K7*K8*K9*K6
       I3=K6
       call EGEMM(I1,I2,I3,X22,F2,Z22)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'234561',
     & 1.000,V3B,Z22)
       deallocate(Z22)
       deallocate(X22)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q80(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q80)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q81(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q80,B2,Q81)
       deallocate(B2)
C
       call sum_stripe(2,shape(X23),size(X23),'21',-1.000,
     & X23,Q81)
       deallocate(Q81)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & N2,M2,M2,N3,X23,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N2-N2,M2-N1,N1-N1,M1-N0,M1-N0,M1-M1/),'123456',t3B1,F2)
       allocate(Z23(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K7*K8*K9*K6
       I3=K0
       call EGEMM(I1,I2,I3,X23,F2,Z23)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'234561',
     & 1.000,V3B,Z23)
       deallocate(Z23)
       deallocate(X23)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S508(M1+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S508)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S508),size(S508),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'2341',S508,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1/),'3412',t2B,D2)
       allocate(S510(M2+1:N3,M2+1:N3,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K6*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S510)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2314',1.000,X2,
     & S510)
       deallocate(S510)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S508),size(S508),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'3421',S508,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S518(M2+1:N3,M1+1:N1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S518)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2413',1.000,X7,
     & S518)
       deallocate(S518)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S508),size(S508),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'3421',S508,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S519(N1+1:M2,M1+1:N1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S519)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2413',1.000,X8,
     & S519)
       deallocate(S519)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S508),size(S508),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'3241',S508,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S548(M2+1:N3,N0+1:N2,N1+1:N3,M1+1:N2))
       I1=K8*K3*K2
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S548)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S548),size(S548),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'2314',S548,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S549(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S549)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',1.000,X2,
     & S549)
       deallocate(S549)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S508),size(S508),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'2341',S508,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S509(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S509)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',1.000,X3,
     & S509)
       deallocate(S509)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q82(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q82)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q82),size(Q82),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q82,B1)
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,M2-N2,M1-N0,M1-N0/),'2134',t2B,D2)
       allocate(S525(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S525)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X82),size(X82),'2341',1.000,
     & X82,S525)
       deallocate(S525)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q82),size(Q82),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q82,B1)
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,M2-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S515(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S515)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S515)
       deallocate(S515)
C
       call sumx_sorted2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M2,N3,M1,N1,M1,N1,X4,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(Z4(M2+1:N3,N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N1,M1+1:N1))
       I1=K7*K7*K6
       I2=K8*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X4,D2,Z4)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134256',
     & -1.000,V3B,Z4)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q82),size(Q82),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q82,B1)
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(S516(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S516)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S516)
       deallocate(S516)
C
       call sumx_sorted2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,M2,M1,N1,M1,N1,X5,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,M2-N2,M2-N1,M1-N0/),'4123',t2B,D2)
       allocate(Z5(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K9
       I2=K8*K6*K6
       I3=K1
       call EGEMM(I1,I2,I3,X5,D2,Z5)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124356',
     & 1.000,V3B,Z5)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'4123',t2B,D2)
       allocate(S513(M2+1:N3,M1+1:N2,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K8*K6
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S513)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S513),size(S513),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'3412',S513,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S514(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S514)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',1.000,X2,
     & S514)
       deallocate(S514)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(S481(M1+1:N2,M1+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S481)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(S481),size(S481),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'4312',S481,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S482(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K2
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S482)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',-1.000,
     & X7,S482)
       deallocate(S482)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(S481),size(S481),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,M1-M1/),'3412',S481,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S520(M2+1:N3,N0+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S520)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X82),size(X82),'2134',-1.000,
     & X82,S520)
       deallocate(S520)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S523(N1+1:M2,M1+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S523)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S523),size(S523),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'3412',S523,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S524(M2+1:N3,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S524)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',-1.000,
     & X10,S524)
       deallocate(S524)
C
       call sumx_sorted2314(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M2,N3,N1,M2,M1,N1,X10,VBHPPP, 1.000)
C
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,M2-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(Z10(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K6
       I2=K7*K8*K6
       I3=K4
       call EGEMM(I1,I2,I3,X10,D2,Z10)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'245136',
     & 1.000,V3B,Z10)
       call sum_stripe(6,shape(V3B),size(V3B),'246135',
     & -1.000,V3B,Z10)
       deallocate(Z10)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S479(M2+1:N3,M1+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S479)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S479),size(S479),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'3412',S479,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S480(M2+1:N3,N2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K4
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S480)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X90),size(X90),'3124',-1.000,
     & X90,S480)
       deallocate(S480)
C
       allocate(D1(N2+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S479),size(S479),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M2-M2,M1-M1/),'4312',S479,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S517(M1+1:N2,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S517)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X82),size(X82),'3124',-1.000,
     & X82,S517)
       deallocate(S517)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z211(M2+1:N3,N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7*K8*K6
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X82,D2,Z211)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'236145',
     & -1.000,V3B,Z211)
       call sum_stripe(6,shape(V3B),size(V3B),'235146',
     & 1.000,V3B,Z211)
       deallocate(Z211)
       deallocate(X82)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S526(M1+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S526)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S526),size(S526),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'3421',S526,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S527(N1+1:M2,M1+1:N1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S527)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X92),size(X92),'2413',-1.000,
     & X92,S527)
       deallocate(S527)
C
       allocate(D2(N0+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,M2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(Z302(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K9
       I2=K7*K6*K6
       I3=K2
       call EGEMM(I1,I2,I3,X92,D2,Z302)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'126345',
     & -1.000,V3B,Z302)
       call sum_stripe(6,shape(V3B),size(V3B),'125346',
     & 1.000,V3B,Z302)
       deallocate(Z302)
       deallocate(X92)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S526),size(S526),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S526,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S528(M2+1:N3,M1+1:N1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S528)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X93),size(X93),'2413',-1.000,
     & X93,S528)
       deallocate(S528)
C
       allocate(D2(N0+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(Z303(M2+1:N3,N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7*K8*K6
       I2=K7*K9*K6
       I3=K2
       call EGEMM(I1,I2,I3,X93,D2,Z303)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'136245',
     & -1.000,V3B,Z303)
       call sum_stripe(6,shape(V3B),size(V3B),'135246',
     & 1.000,V3B,Z303)
       deallocate(Z303)
       deallocate(X93)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4132',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S531(M2+1:N3,M1+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S531)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S531),size(S531),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'3412',S531,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S532(M2+1:N3,N2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S532)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X90),size(X90),'2134',-1.000,
     & X90,S532)
       deallocate(S532)
C
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(Z298(N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K6
       I2=K7*K8*K9
       I3=K4
       call EGEMM(I1,I2,I3,X90,D2,Z298)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'346125',
     & -1.000,V3B,Z298)
       call sum_stripe(6,shape(V3B),size(V3B),'345126',
     & 1.000,V3B,Z298)
       deallocate(Z298)
       deallocate(X90)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q83(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q83)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q83),size(Q83),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q83,B1)
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(S534(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S534)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S534)
       deallocate(S534)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q83),size(Q83),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q83,B1)
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,M2-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(S533(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K6
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S533)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S533)
       deallocate(S533)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S451(M2+1:N3,M1+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S451)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S451),size(S451),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'3412',S451,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S455(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S455)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'3124',-1.000,
     & X3,S455)
       deallocate(S455)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S451),size(S451),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'3412',S451,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S452(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S452)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'3124',-1.000,
     & X2,S452)
       deallocate(S452)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S475(N1+1:M2,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S475)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S475),size(S475),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'3412',S475,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S476(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S476)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X89),size(X89),'2134',-1.000,
     & X89,S476)
       deallocate(S476)
C
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,M2-N2,M1-N0,M1-N0/),'2134',t2B,D2)
       allocate(Z296(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K6
       I2=K7*K8*K6
       I3=K3
       call EGEMM(I1,I2,I3,X89,D2,Z296)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'146235',
     & -1.000,V3B,Z296)
       call sum_stripe(6,shape(V3B),size(V3B),'145236',
     & 1.000,V3B,Z296)
       deallocate(Z296)
       deallocate(X89)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q73(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q73),size(Q73),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q73,B1)
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,M2-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(S484(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K6
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S484)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S484)
       deallocate(S484)
C
       call sumx_sorted1234(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M2,N3,M1,N2,M1,N1,X7,VBHHPH, 1.000)
C
       allocate(D2(N0+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(Z7(M2+1:N3,N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7*K8*K6
       I2=K7*K9*K6
       I3=K2
       call EGEMM(I1,I2,I3,X7,D2,Z7)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'135246',
     & 1.000,V3B,Z7)
       call sum_stripe(6,shape(V3B),size(V3B),'136245',
     & -1.000,V3B,Z7)
       deallocate(Z7)
       deallocate(X7)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q73),size(Q73),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q73,B1)
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(S486(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S486)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S486)
       deallocate(S486)
C
       call sumx_sorted1234(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,M2,M1,N2,M1,N1,X8,VBHHPH, 1.000)
C
       allocate(D2(N0+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,M2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(Z8(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K9
       I2=K7*K6*K6
       I3=K2
       call EGEMM(I1,I2,I3,X8,D2,Z8)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'125346',
     & -1.000,V3B,Z8)
       call sum_stripe(6,shape(V3B),size(V3B),'126345',
     & 1.000,V3B,Z8)
       deallocate(Z8)
       deallocate(X8)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S521(M2+1:N3,M1+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S521)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S521),size(S521),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'3412',S521,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S522(M2+1:N3,N2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S522)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',-1.000,
     & X9,S522)
       deallocate(S522)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S398(M2+1:N3,N0+1:N2,N2+1:N3,M1+1:N1))
       I1=K7*K4*K2
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S398)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S398),size(S398),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'2314',S398,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S399(M2+1:N3,N2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S399)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',1.000,X9,
     & S399)
       deallocate(S399)
C
       call sumx_sorted2314(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M2,N3,M2,N3,M1,N1,X9,VBHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(Z9(N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K6
       I2=K7*K8*K9
       I3=K4
       call EGEMM(I1,I2,I3,X9,D2,Z9)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'345126',
     & -1.000,V3B,Z9)
       call sum_stripe(6,shape(V3B),size(V3B),'346125',
     & 1.000,V3B,Z9)
       deallocate(Z9)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S529(N1+1:M2,M1+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S529)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S529),size(S529),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'3412',S529,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S530(M2+1:N3,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S530)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X91),size(X91),'2134',1.000,
     & X91,S530)
       deallocate(S530)
C
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,M2-N1,M1-N0,M1-N0/),'1234',t2B,D2)
       allocate(Z300(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K6
       I2=K7*K8*K6
       I3=K4
       call EGEMM(I1,I2,I3,X91,D2,Z300)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'246135',
     & 1.000,V3B,Z300)
       call sum_stripe(6,shape(V3B),size(V3B),'245136',
     & -1.000,V3B,Z300)
       deallocate(Z300)
       deallocate(X91)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S511(N1+1:M2,M1+1:N2,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S511)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S511),size(S511),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'3412',S511,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S512(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S512)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',1.000,X3,
     & S512)
       deallocate(S512)
C
       call sumx_sorted2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M2,N3,N1,M2,M1,N2,X3,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,M2-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(Z3(M2+1:N3,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K6
       I2=K7*K7*K6
       I3=K3
       call EGEMM(I1,I2,I3,X3,D2,Z3)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'256134',
     & -1.000,V3B,Z3)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,M2-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S488(M1+1:N2,N0+1:N2,N1+1:N3,M2+1:N3))
       I1=K6*K3*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S488)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S488),size(S488),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'2341',S488,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S489(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S489)
       deallocate(D1)
       deallocate(B2)
       deallocate(S488)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S489)
       deallocate(S489)
C
       call sumx_sorted2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M2,N3,M2,N3,M1,N2,X2,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',t2A,D2)
       allocate(Z2(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K6
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,X2,D2,Z2)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'356124',
     & 1.000,V3B,Z2)
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'1324',VBHPPH,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,M2-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z63(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K8*K6*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z63)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124536',
     & -1.000,V3B,Z63)
       call sum_stripe(6,shape(V3B),size(V3B),'124635',
     & 1.000,V3B,Z63)
       deallocate(Z63)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,M1-N0/),'1324',VBHPPH,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,M2-N1,M1-M1,M1-N0/),'421356',t3C1,F2)
       allocate(Z64(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K8*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z64)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'124536',
     & -1.000,V3B,Z64)
       call sum_stripe(6,shape(V3B),size(V3B),'124635',
     & 1.000,V3B,Z64)
       deallocate(Z64)
C
       call sumx_sorted3(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M2,N3,M2,N3,N1,M2,M1,N2,M1,N1,M1,N1,HT3B1,V3B,1.0)
       deallocate(V3B)
C
       end
