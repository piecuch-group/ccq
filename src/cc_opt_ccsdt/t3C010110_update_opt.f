       subroutine t3C010110_update(N0,N1,N2,N3,HT3C1,shift,
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
       real*8 HT3C1(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:N2,M1+1:N2,N0+1:N1)
C
       real*8,allocatable::V3C(:,:,:,:,:,:)
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
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
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
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q15(:,:)
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
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q22(:,:)
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
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q24(:,:)
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
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::S145(:,:,:,:)
       real*8,allocatable::S146(:,:,:,:)
       real*8,allocatable::S147(:,:,:,:)
       real*8,allocatable::S148(:,:,:,:)
       real*8,allocatable::S149(:,:,:,:)
       real*8,allocatable::S150(:,:,:,:)
       real*8,allocatable::Q27(:,:)
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
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::Q29(:,:)
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
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::S176(:,:,:,:)
       real*8,allocatable::S177(:,:,:,:)
       real*8,allocatable::S178(:,:,:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::S179(:,:,:,:)
       real*8,allocatable::S180(:,:,:,:)
       real*8,allocatable::S181(:,:,:,:)
       real*8,allocatable::S182(:,:,:,:)
       real*8,allocatable::S183(:,:,:,:)
       real*8,allocatable::S184(:,:,:,:)
       real*8,allocatable::S185(:,:,:,:)
       real*8,allocatable::S186(:,:,:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q36(:,:)
       real*8,allocatable::Q37(:,:)
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
       real*8,allocatable::S298(:,:,:,:)
       real*8,allocatable::S299(:,:,:,:)
       real*8,allocatable::S300(:,:,:,:)
       real*8,allocatable::S301(:,:,:,:)
       real*8,allocatable::S302(:,:,:,:)
       real*8,allocatable::S303(:,:,:,:)
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
       real*8,allocatable::S337(:,:,:,:)
       real*8,allocatable::S338(:,:,:,:)
       real*8,allocatable::S339(:,:,:,:)
       real*8,allocatable::S340(:,:,:,:)
       real*8,allocatable::S341(:,:,:,:)
       real*8,allocatable::S342(:,:,:,:)
       real*8,allocatable::S343(:,:,:,:)
       real*8,allocatable::S344(:,:,:,:)
       real*8,allocatable::S345(:,:,:,:)
       real*8,allocatable::S346(:,:,:,:)
       real*8,allocatable::S347(:,:,:,:)
       real*8,allocatable::S348(:,:,:,:)
       real*8,allocatable::S349(:,:,:,:)
       real*8,allocatable::S350(:,:,:,:)
       real*8,allocatable::S351(:,:,:,:)
       real*8,allocatable::S352(:,:,:,:)
       real*8,allocatable::S353(:,:,:,:)
       real*8,allocatable::S354(:,:,:,:)
       real*8,allocatable::S355(:,:,:,:)
       real*8,allocatable::S356(:,:,:,:)
       real*8,allocatable::S357(:,:,:,:)
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
       real*8,allocatable::S372(:,:,:,:)
       real*8,allocatable::S373(:,:,:,:)
       real*8,allocatable::S374(:,:,:,:)
       real*8,allocatable::S375(:,:,:,:)
       real*8,allocatable::S376(:,:,:,:)
       real*8,allocatable::S377(:,:,:,:)
       real*8,allocatable::S378(:,:,:,:)
       real*8,allocatable::S379(:,:,:,:)
       real*8,allocatable::S380(:,:,:,:)
       real*8,allocatable::S381(:,:,:,:)
       real*8,allocatable::S382(:,:,:,:)
       real*8,allocatable::S383(:,:,:,:)
       real*8,allocatable::S384(:,:,:,:)
       real*8,allocatable::Q38(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::S385(:,:,:,:)
       real*8,allocatable::S386(:,:,:,:)
       real*8,allocatable::S387(:,:,:,:)
       real*8,allocatable::S388(:,:,:,:)
       real*8,allocatable::S389(:,:,:,:)
       real*8,allocatable::S390(:,:,:,:)
       real*8,allocatable::S391(:,:,:,:)
       real*8,allocatable::S392(:,:,:,:)
       real*8,allocatable::S393(:,:,:,:)
       real*8,allocatable::S394(:,:,:,:)
       real*8,allocatable::S395(:,:,:,:)
       real*8,allocatable::S396(:,:,:,:)
       real*8,allocatable::S397(:,:,:,:)
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::S398(:,:,:,:)
       real*8,allocatable::S399(:,:,:,:)
       real*8,allocatable::S400(:,:,:,:)
       real*8,allocatable::S401(:,:,:,:)
       real*8,allocatable::S402(:,:,:,:)
       real*8,allocatable::S403(:,:,:,:)
       real*8,allocatable::S404(:,:,:,:)
       real*8,allocatable::S405(:,:,:,:)
       real*8,allocatable::S406(:,:,:,:)
       real*8,allocatable::S407(:,:,:,:)
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
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::S424(:,:,:,:)
       real*8,allocatable::S425(:,:,:,:)
       real*8,allocatable::S426(:,:,:,:)
       real*8,allocatable::S427(:,:,:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q44(:,:)
       real*8,allocatable::S428(:,:,:,:)
       real*8,allocatable::S429(:,:,:,:)
       real*8,allocatable::S430(:,:,:,:)
       real*8,allocatable::Q45(:,:)
       real*8,allocatable::Q46(:,:)
       real*8,allocatable::Q47(:,:)
       real*8,allocatable::Q48(:,:)
       real*8,allocatable::S431(:,:,:,:)
       real*8,allocatable::S432(:,:,:,:)
       real*8,allocatable::S433(:,:,:,:)
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
       real*8,allocatable::S446(:,:,:,:)
       real*8,allocatable::S447(:,:,:,:)
       real*8,allocatable::S448(:,:,:,:)
       real*8,allocatable::S449(:,:,:,:)
       real*8,allocatable::S450(:,:,:,:)
       real*8,allocatable::S451(:,:,:,:)
       real*8,allocatable::S452(:,:,:,:)
       real*8,allocatable::S453(:,:,:,:)
       real*8,allocatable::S454(:,:,:,:)
       real*8,allocatable::S455(:,:,:,:)
       real*8,allocatable::S456(:,:,:,:)
       real*8,allocatable::S457(:,:,:,:)
       real*8,allocatable::S458(:,:,:,:)
       real*8,allocatable::S459(:,:,:,:)
       real*8,allocatable::S460(:,:,:,:)
       real*8,allocatable::S461(:,:,:,:)
       real*8,allocatable::S462(:,:,:,:)
       real*8,allocatable::S463(:,:,:,:)
       real*8,allocatable::S464(:,:,:,:)
       real*8,allocatable::S465(:,:,:,:)
       real*8,allocatable::S466(:,:,:,:)
       real*8,allocatable::S467(:,:,:,:)
       real*8,allocatable::S468(:,:,:,:)
       real*8,allocatable::S469(:,:,:,:)
       real*8,allocatable::S470(:,:,:,:)
       real*8,allocatable::S471(:,:,:,:)
       real*8,allocatable::S472(:,:,:,:)
       real*8,allocatable::S473(:,:,:,:)
       real*8,allocatable::S474(:,:,:,:)
       real*8,allocatable::S475(:,:,:,:)
       real*8,allocatable::S476(:,:,:,:)
       real*8,allocatable::S477(:,:,:,:)
       real*8,allocatable::S478(:,:,:,:)
       real*8,allocatable::S479(:,:,:,:)
       real*8,allocatable::S480(:,:,:,:)
       real*8,allocatable::S481(:,:,:,:)
       real*8,allocatable::S482(:,:,:,:)
       real*8,allocatable::S483(:,:,:,:)
       real*8,allocatable::S484(:,:,:,:)
       real*8,allocatable::S485(:,:,:,:)
       real*8,allocatable::S486(:,:,:,:)
       real*8,allocatable::S487(:,:,:,:)
       real*8,allocatable::S488(:,:,:,:)
       real*8,allocatable::S489(:,:,:,:)
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
       real*8,allocatable::Q49(:,:)
       real*8,allocatable::S508(:,:,:,:)
       real*8,allocatable::S509(:,:,:,:)
       real*8,allocatable::S510(:,:,:,:)
       real*8,allocatable::S511(:,:,:,:)
       real*8,allocatable::S512(:,:,:,:)
       real*8,allocatable::S513(:,:,:,:)
       real*8,allocatable::S514(:,:,:,:)
       real*8,allocatable::S515(:,:,:,:)
       real*8,allocatable::S516(:,:,:,:)
       real*8,allocatable::S517(:,:,:,:)
       real*8,allocatable::S518(:,:,:,:)
       real*8,allocatable::S519(:,:,:,:)
       real*8,allocatable::S520(:,:,:,:)
       real*8,allocatable::S521(:,:,:,:)
       real*8,allocatable::Q50(:,:)
       real*8,allocatable::Q51(:,:)
       real*8,allocatable::S522(:,:,:,:)
       real*8,allocatable::S523(:,:,:,:)
       real*8,allocatable::S524(:,:,:,:)
       real*8,allocatable::Q52(:,:)
       real*8,allocatable::Q53(:,:)
       real*8,allocatable::Q54(:,:)
       real*8,allocatable::Q55(:,:)
       real*8,allocatable::S525(:,:,:,:)
       real*8,allocatable::S549(:,:,:,:)
       real*8,allocatable::S550(:,:,:,:)
       real*8,allocatable::S526(:,:,:,:)
       real*8,allocatable::S531(:,:,:,:)
       real*8,allocatable::S552(:,:,:,:)
       real*8,allocatable::S556(:,:,:,:)
       real*8,allocatable::S532(:,:,:,:)
       real*8,allocatable::S537(:,:,:,:)
       real*8,allocatable::S538(:,:,:,:)
       real*8,allocatable::S539(:,:,:,:)
       real*8,allocatable::S540(:,:,:,:)
       real*8,allocatable::Q56(:,:)
       real*8,allocatable::Q57(:,:)
       real*8,allocatable::Q58(:,:)
       real*8,allocatable::Q59(:,:)
       real*8,allocatable::Q60(:,:)
       real*8,allocatable::Q61(:,:)
       real*8,allocatable::Q62(:,:)
       real*8,allocatable::Q63(:,:)
       real*8,allocatable::S541(:,:,:,:)
       real*8,allocatable::S542(:,:,:,:)
       real*8,allocatable::S543(:,:,:,:)
       real*8,allocatable::S544(:,:,:,:)
       real*8,allocatable::S545(:,:,:,:)
       real*8,allocatable::S546(:,:,:,:)
       real*8,allocatable::S547(:,:,:,:)
       real*8,allocatable::S548(:,:,:,:)
       real*8,allocatable::S527(:,:,:,:)
       real*8,allocatable::S553(:,:,:,:)
       real*8,allocatable::S554(:,:,:,:)
       real*8,allocatable::S528(:,:,:,:)
       real*8,allocatable::S533(:,:,:,:)
       real*8,allocatable::S534(:,:,:,:)
       real*8,allocatable::S529(:,:,:,:)
       real*8,allocatable::S551(:,:,:,:)
       real*8,allocatable::S555(:,:,:,:)
       real*8,allocatable::S530(:,:,:,:)
       real*8,allocatable::S535(:,:,:,:)
       real*8,allocatable::S536(:,:,:,:)
       real*8,allocatable::S557(:,:,:,:)
       real*8,allocatable::S559(:,:,:,:)
       real*8,allocatable::S558(:,:,:,:)
       real*8,allocatable::S560(:,:,:,:)
       real*8,allocatable::S561(:,:,:,:)
       real*8,allocatable::S562(:,:,:,:)
       real*8,allocatable::S563(:,:,:,:)
       real*8,allocatable::S564(:,:,:,:)
       real*8,allocatable::S566(:,:,:,:)
       real*8,allocatable::S565(:,:,:,:)
       real*8,allocatable::S567(:,:,:,:)
       real*8,allocatable::S568(:,:,:,:)
       real*8,allocatable::S569(:,:,:,:)
       real*8,allocatable::S570(:,:,:,:)
       real*8,allocatable::S571(:,:,:,:)
       real*8,allocatable::S667(:,:,:,:)
       real*8,allocatable::S668(:,:,:,:)
       real*8,allocatable::S572(:,:,:,:)
       real*8,allocatable::S575(:,:,:,:)
       real*8,allocatable::S576(:,:,:,:)
       real*8,allocatable::S577(:,:,:,:)
       real*8,allocatable::S578(:,:,:,:)
       real*8,allocatable::S581(:,:,:,:)
       real*8,allocatable::S582(:,:,:,:)
       real*8,allocatable::S579(:,:,:,:)
       real*8,allocatable::S580(:,:,:,:)
       real*8,allocatable::S585(:,:,:,:)
       real*8,allocatable::S592(:,:,:,:)
       real*8,allocatable::S586(:,:,:,:)
       real*8,allocatable::S589(:,:,:,:)
       real*8,allocatable::S590(:,:,:,:)
       real*8,allocatable::S583(:,:,:,:)
       real*8,allocatable::S584(:,:,:,:)
       real*8,allocatable::S591(:,:,:,:)
       real*8,allocatable::S587(:,:,:,:)
       real*8,allocatable::S588(:,:,:,:)
       real*8,allocatable::S593(:,:,:,:)
       real*8,allocatable::S596(:,:,:,:)
       real*8,allocatable::S603(:,:,:,:)
       real*8,allocatable::S597(:,:,:,:)
       real*8,allocatable::S600(:,:,:,:)
       real*8,allocatable::S605(:,:,:,:)
       real*8,allocatable::S601(:,:,:,:)
       real*8,allocatable::S598(:,:,:,:)
       real*8,allocatable::S604(:,:,:,:)
       real*8,allocatable::S599(:,:,:,:)
       real*8,allocatable::S594(:,:,:,:)
       real*8,allocatable::S595(:,:,:,:)
       real*8,allocatable::S602(:,:,:,:)
       real*8,allocatable::Q66(:,:)
       real*8,allocatable::Q67(:,:)
       real*8,allocatable::S606(:,:,:,:)
       real*8,allocatable::S607(:,:,:,:)
       real*8,allocatable::S608(:,:,:,:)
       real*8,allocatable::S609(:,:,:,:)
       real*8,allocatable::Q68(:,:)
       real*8,allocatable::Q72(:,:)
       real*8,allocatable::Q69(:,:)
       real*8,allocatable::S612(:,:,:,:)
       real*8,allocatable::S613(:,:,:,:)
       real*8,allocatable::S614(:,:,:,:)
       real*8,allocatable::S615(:,:,:,:)
       real*8,allocatable::S616(:,:,:,:)
       real*8,allocatable::S617(:,:,:,:)
       real*8,allocatable::Q70(:,:)
       real*8,allocatable::Q71(:,:)
       real*8,allocatable::S610(:,:,:,:)
       real*8,allocatable::S611(:,:,:,:)
       real*8,allocatable::S618(:,:,:,:)
       real*8,allocatable::S619(:,:,:,:)
       real*8,allocatable::S620(:,:,:,:)
       real*8,allocatable::S626(:,:,:,:)
       real*8,allocatable::S621(:,:,:,:)
       real*8,allocatable::S624(:,:,:,:)
       real*8,allocatable::S628(:,:,:,:)
       real*8,allocatable::S625(:,:,:,:)
       real*8,allocatable::Q75(:,:)
       real*8,allocatable::Q78(:,:)
       real*8,allocatable::Q76(:,:)
       real*8,allocatable::S622(:,:,:,:)
       real*8,allocatable::S623(:,:,:,:)
       real*8,allocatable::S627(:,:,:,:)
       real*8,allocatable::Q73(:,:)
       real*8,allocatable::Q74(:,:)
       real*8,allocatable::Q77(:,:)
       real*8,allocatable::Q79(:,:)
       real*8,allocatable::Q80(:,:)
       real*8,allocatable::Q81(:,:)
       real*8,allocatable::Q82(:,:)
       real*8,allocatable::S629(:,:,:,:)
       real*8,allocatable::S631(:,:,:,:)
       real*8,allocatable::S632(:,:,:,:)
       real*8,allocatable::S633(:,:,:,:)
       real*8,allocatable::S630(:,:,:,:)
       real*8,allocatable::S634(:,:,:,:)
       real*8,allocatable::S635(:,:,:,:)
       real*8,allocatable::Q83(:,:)
       real*8,allocatable::S639(:,:,:,:)
       real*8,allocatable::S641(:,:,:,:)
       real*8,allocatable::S643(:,:,:,:)
       real*8,allocatable::S636(:,:,:,:)
       real*8,allocatable::S637(:,:,:,:)
       real*8,allocatable::S642(:,:,:,:)
       real*8,allocatable::S640(:,:,:,:)
       real*8,allocatable::S638(:,:,:,:)
       real*8,allocatable::S644(:,:,:,:)
       real*8,allocatable::S646(:,:,:,:)
       real*8,allocatable::S647(:,:,:,:)
       real*8,allocatable::S648(:,:,:,:)
       real*8,allocatable::S649(:,:,:,:)
       real*8,allocatable::S650(:,:,:,:)
       real*8,allocatable::S651(:,:,:,:)
       real*8,allocatable::S652(:,:,:,:)
       real*8,allocatable::S812(:,:,:,:)
       real*8,allocatable::S813(:,:,:,:)
       real*8,allocatable::S816(:,:,:,:)
       real*8,allocatable::S818(:,:,:,:)
       real*8,allocatable::S814(:,:,:,:)
       real*8,allocatable::S815(:,:,:,:)
       real*8,allocatable::S645(:,:,:,:)
       real*8,allocatable::S819(:,:,:,:)
       real*8,allocatable::S820(:,:,:,:)
       real*8,allocatable::S817(:,:,:,:)
       real*8,allocatable::S821(:,:,:,:)
       real*8,allocatable::S822(:,:,:,:)
       real*8,allocatable::S655(:,:,:,:)
       real*8,allocatable::S665(:,:,:,:)
       real*8,allocatable::S656(:,:,:,:)
       real*8,allocatable::S657(:,:,:,:)
       real*8,allocatable::S659(:,:,:,:)
       real*8,allocatable::S660(:,:,:,:)
       real*8,allocatable::S661(:,:,:,:)
       real*8,allocatable::S823(:,:,:,:)
       real*8,allocatable::S824(:,:,:,:)
       real*8,allocatable::S826(:,:,:,:)
       real*8,allocatable::S827(:,:,:,:)
       real*8,allocatable::S663(:,:,:,:)
       real*8,allocatable::S825(:,:,:,:)
       real*8,allocatable::S828(:,:,:,:)
       real*8,allocatable::S674(:,:,:,:)
       real*8,allocatable::S675(:,:,:,:)
       real*8,allocatable::S676(:,:,:,:)
       real*8,allocatable::S677(:,:,:,:)
       real*8,allocatable::S678(:,:,:,:)
       real*8,allocatable::S679(:,:,:,:)
       real*8,allocatable::S682(:,:,:,:)
       real*8,allocatable::S684(:,:,:,:)
       real*8,allocatable::S683(:,:,:,:)
       real*8,allocatable::S685(:,:,:,:)
       real*8,allocatable::S689(:,:,:,:)
       real*8,allocatable::S686(:,:,:,:)
       real*8,allocatable::S687(:,:,:,:)
       real*8,allocatable::S690(:,:,:,:)
       real*8,allocatable::S688(:,:,:,:)
       real*8,allocatable::S691(:,:,:,:)
       real*8,allocatable::S692(:,:,:,:)
       real*8,allocatable::S693(:,:,:,:)
       real*8,allocatable::S694(:,:,:,:)
       real*8,allocatable::S695(:,:,:,:)
       real*8,allocatable::S696(:,:,:,:)
       real*8,allocatable::S697(:,:,:,:)
       real*8,allocatable::S701(:,:,:,:)
       real*8,allocatable::S698(:,:,:,:)
       real*8,allocatable::S699(:,:,:,:)
       real*8,allocatable::S702(:,:,:,:)
       real*8,allocatable::S700(:,:,:,:)
       real*8,allocatable::S703(:,:,:,:)
       real*8,allocatable::S711(:,:,:,:)
       real*8,allocatable::S704(:,:,:,:)
       real*8,allocatable::S707(:,:,:,:)
       real*8,allocatable::S713(:,:,:,:)
       real*8,allocatable::S708(:,:,:,:)
       real*8,allocatable::S705(:,:,:,:)
       real*8,allocatable::S712(:,:,:,:)
       real*8,allocatable::S706(:,:,:,:)
       real*8,allocatable::S709(:,:,:,:)
       real*8,allocatable::S714(:,:,:,:)
       real*8,allocatable::S710(:,:,:,:)
       real*8,allocatable::S715(:,:,:,:)
       real*8,allocatable::S716(:,:,:,:)
       real*8,allocatable::S717(:,:,:,:)
       real*8,allocatable::S718(:,:,:,:)
       real*8,allocatable::S719(:,:,:,:)
       real*8,allocatable::S723(:,:,:,:)
       real*8,allocatable::S720(:,:,:,:)
       real*8,allocatable::S721(:,:,:,:)
       real*8,allocatable::S724(:,:,:,:)
       real*8,allocatable::S722(:,:,:,:)
       real*8,allocatable::Q85(:,:)
       real*8,allocatable::Q89(:,:)
       real*8,allocatable::Q86(:,:)
       real*8,allocatable::S727(:,:,:,:)
       real*8,allocatable::S734(:,:,:,:)
       real*8,allocatable::S728(:,:,:,:)
       real*8,allocatable::S731(:,:,:,:)
       real*8,allocatable::S736(:,:,:,:)
       real*8,allocatable::S732(:,:,:,:)
       real*8,allocatable::S729(:,:,:,:)
       real*8,allocatable::S735(:,:,:,:)
       real*8,allocatable::S730(:,:,:,:)
       real*8,allocatable::S725(:,:,:,:)
       real*8,allocatable::S726(:,:,:,:)
       real*8,allocatable::S733(:,:,:,:)
       real*8,allocatable::Q87(:,:)
       real*8,allocatable::Q88(:,:)
       real*8,allocatable::S737(:,:,:,:)
       real*8,allocatable::S738(:,:,:,:)
       real*8,allocatable::S739(:,:,:,:)
       real*8,allocatable::S740(:,:,:,:)
       real*8,allocatable::S741(:,:,:,:)
       real*8,allocatable::S742(:,:,:,:)
       real*8,allocatable::Q90(:,:)
       real*8,allocatable::Q94(:,:)
       real*8,allocatable::Q91(:,:)
       real*8,allocatable::Q92(:,:)
       real*8,allocatable::Q95(:,:)
       real*8,allocatable::Q93(:,:)
       real*8,allocatable::S743(:,:,:,:)
       real*8,allocatable::S750(:,:,:,:)
       real*8,allocatable::S751(:,:,:,:)
       real*8,allocatable::S752(:,:,:,:)
       real*8,allocatable::S753(:,:,:,:)
       real*8,allocatable::S754(:,:,:,:)
       real*8,allocatable::S755(:,:,:,:)
       real*8,allocatable::S744(:,:,:,:)
       real*8,allocatable::S745(:,:,:,:)
       real*8,allocatable::S756(:,:,:,:)
       real*8,allocatable::S757(:,:,:,:)
       real*8,allocatable::S770(:,:,:,:)
       real*8,allocatable::S771(:,:,:,:)
       real*8,allocatable::S772(:,:,:,:)
       real*8,allocatable::S773(:,:,:,:)
       real*8,allocatable::S746(:,:,:,:)
       real*8,allocatable::S762(:,:,:,:)
       real*8,allocatable::S768(:,:,:,:)
       real*8,allocatable::S763(:,:,:,:)
       real*8,allocatable::Q96(:,:)
       real*8,allocatable::S769(:,:,:,:)
       real*8,allocatable::S781(:,:,:,:)
       real*8,allocatable::S784(:,:,:,:)
       real*8,allocatable::S765(:,:,:,:)
       real*8,allocatable::S747(:,:,:,:)
       real*8,allocatable::S749(:,:,:,:)
       real*8,allocatable::S748(:,:,:,:)
       real*8,allocatable::S758(:,:,:,:)
       real*8,allocatable::S764(:,:,:,:)
       real*8,allocatable::S759(:,:,:,:)
       real*8,allocatable::S774(:,:,:,:)
       real*8,allocatable::S775(:,:,:,:)
       real*8,allocatable::S776(:,:,:,:)
       real*8,allocatable::S777(:,:,:,:)
       real*8,allocatable::S778(:,:,:,:)
       real*8,allocatable::S779(:,:,:,:)
       real*8,allocatable::S766(:,:,:,:)
       real*8,allocatable::S782(:,:,:,:)
       real*8,allocatable::S783(:,:,:,:)
       real*8,allocatable::S767(:,:,:,:)
       real*8,allocatable::S780(:,:,:,:)
       real*8,allocatable::S760(:,:,:,:)
       real*8,allocatable::S761(:,:,:,:)
       real*8,allocatable::S785(:,:,:,:)
       real*8,allocatable::S787(:,:,:,:)
       real*8,allocatable::S788(:,:,:,:)
       real*8,allocatable::S789(:,:,:,:)
       real*8,allocatable::S829(:,:,:,:)
       real*8,allocatable::S830(:,:,:,:)
       real*8,allocatable::S832(:,:,:,:)
       real*8,allocatable::S833(:,:,:,:)
       real*8,allocatable::S786(:,:,:,:)
       real*8,allocatable::S831(:,:,:,:)
       real*8,allocatable::Q97(:,:)
       real*8,allocatable::S803(:,:,:,:)
       real*8,allocatable::S809(:,:,:,:)
       real*8,allocatable::S811(:,:,:,:)
       real*8,allocatable::S796(:,:,:,:)
       real*8,allocatable::S800(:,:,:,:)
       real*8,allocatable::S801(:,:,:,:)
       real*8,allocatable::S794(:,:,:,:)
       real*8,allocatable::S808(:,:,:,:)
       real*8,allocatable::S810(:,:,:,:)
       real*8,allocatable::S795(:,:,:,:)
       real*8,allocatable::S802(:,:,:,:)
       real*8,allocatable::S790(:,:,:,:)
       real*8,allocatable::S798(:,:,:,:)
       real*8,allocatable::S799(:,:,:,:)
       real*8,allocatable::S791(:,:,:,:)
       real*8,allocatable::S834(:,:,:,:)
       real*8,allocatable::S835(:,:,:,:)
       real*8,allocatable::S797(:,:,:,:)
       real*8,allocatable::Q84(:,:)
       real*8,allocatable::S666(:,:,:,:)
       real*8,allocatable::S658(:,:,:,:)
       real*8,allocatable::S662(:,:,:,:)
       real*8,allocatable::S664(:,:,:,:)
       real*8,allocatable::Q64(:,:)
       real*8,allocatable::Q65(:,:)
       real*8,allocatable::S573(:,:,:,:)
       real*8,allocatable::S574(:,:,:,:)
       real*8,allocatable::S670(:,:,:,:)
       real*8,allocatable::S669(:,:,:,:)
       real*8,allocatable::S671(:,:,:,:)
       real*8,allocatable::S673(:,:,:,:)
       real*8,allocatable::S672(:,:,:,:)
       real*8,allocatable::S680(:,:,:,:)
       real*8,allocatable::S681(:,:,:,:)
       real*8,allocatable::S804(:,:,:,:)
       real*8,allocatable::S805(:,:,:,:)
       real*8,allocatable::S792(:,:,:,:)
       real*8,allocatable::S793(:,:,:,:)
       real*8,allocatable::S806(:,:,:,:)
       real*8,allocatable::S807(:,:,:,:)
       real*8,allocatable::S653(:,:,:,:)
       real*8,allocatable::S654(:,:,:,:)
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
       real*8,allocatable::X27(:,:,:,:)
       real*8,allocatable::Z27(:,:,:,:,:,:)
       real*8,allocatable::X28(:,:,:,:)
       real*8,allocatable::Z28(:,:,:,:,:,:)
       real*8,allocatable::X29(:,:)
       real*8,allocatable::Z29(:,:,:,:,:,:)
       real*8,allocatable::X30(:,:)
       real*8,allocatable::Z30(:,:,:,:,:,:)
       real*8,allocatable::X31(:,:)
       real*8,allocatable::Z31(:,:,:,:,:,:)
       real*8,allocatable::X32(:,:)
       real*8,allocatable::Z32(:,:,:,:,:,:)
       real*8,allocatable::X33(:,:)
       real*8,allocatable::Z33(:,:,:,:,:,:)
       real*8,allocatable::X34(:,:)
       real*8,allocatable::Z34(:,:,:,:,:,:)
       real*8,allocatable::X35(:,:)
       real*8,allocatable::Z35(:,:,:,:,:,:)
       real*8,allocatable::X36(:,:)
       real*8,allocatable::Z36(:,:,:,:,:,:)
       real*8,allocatable::X37(:,:)
       real*8,allocatable::Z37(:,:,:,:,:,:)
       real*8,allocatable::X38(:,:)
       real*8,allocatable::Z38(:,:,:,:,:,:)
       real*8,allocatable::X39(:,:)
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
       real*8,allocatable::X63(:,:,:,:)
       real*8,allocatable::Z63(:,:,:,:,:,:)
       real*8,allocatable::X64(:,:,:,:)
       real*8,allocatable::Z64(:,:,:,:,:,:)
       real*8,allocatable::X65(:,:,:,:)
       real*8,allocatable::Z65(:,:,:,:,:,:)
       real*8,allocatable::X66(:,:,:,:)
       real*8,allocatable::Z66(:,:,:,:,:,:)
       real*8,allocatable::X67(:,:,:,:)
       real*8,allocatable::Z67(:,:,:,:,:,:)
       real*8,allocatable::X68(:,:,:,:)
       real*8,allocatable::Z68(:,:,:,:,:,:)
       real*8,allocatable::X69(:,:,:,:)
       real*8,allocatable::Z69(:,:,:,:,:,:)
       real*8,allocatable::X70(:,:,:,:)
       real*8,allocatable::Z70(:,:,:,:,:,:)
       real*8,allocatable::X71(:,:,:,:)
       real*8,allocatable::Z71(:,:,:,:,:,:)
       real*8,allocatable::X72(:,:,:,:)
       real*8,allocatable::Z72(:,:,:,:,:,:)
       real*8,allocatable::X73(:,:,:,:)
       real*8,allocatable::Z73(:,:,:,:,:,:)
       real*8,allocatable::X74(:,:,:,:)
       real*8,allocatable::Z74(:,:,:,:,:,:)
       real*8,allocatable::X75(:,:,:,:)
       real*8,allocatable::Z75(:,:,:,:,:,:)
       real*8,allocatable::X76(:,:,:,:)
       real*8,allocatable::Z76(:,:,:,:,:,:)
       real*8,allocatable::X77(:,:,:,:)
       real*8,allocatable::Z77(:,:,:,:,:,:)
       real*8,allocatable::X78(:,:,:,:)
       real*8,allocatable::Z78(:,:,:,:,:,:)
       real*8,allocatable::X79(:,:,:,:)
       real*8,allocatable::Z79(:,:,:,:,:,:)
       real*8,allocatable::X80(:,:,:,:)
       real*8,allocatable::Z80(:,:,:,:,:,:)
       real*8,allocatable::X81(:,:,:,:)
       real*8,allocatable::Z81(:,:,:,:,:,:)
       real*8,allocatable::X82(:,:,:,:)
       real*8,allocatable::Z82(:,:,:,:,:,:)
       real*8,allocatable::X83(:,:,:,:)
       real*8,allocatable::Z83(:,:,:,:,:,:)
       real*8,allocatable::X84(:,:,:,:)
       real*8,allocatable::Z84(:,:,:,:,:,:)
       real*8,allocatable::X85(:,:,:,:)
       real*8,allocatable::Z85(:,:,:,:,:,:)
       real*8,allocatable::X86(:,:,:,:)
       real*8,allocatable::Z86(:,:,:,:,:,:)
       real*8,allocatable::X87(:,:,:,:)
       real*8,allocatable::Z87(:,:,:,:,:,:)
       real*8,allocatable::X88(:,:,:,:)
       real*8,allocatable::Z88(:,:,:,:,:,:)
       real*8,allocatable::Z89(:,:,:,:,:,:)
       real*8,allocatable::Z90(:,:,:,:,:,:)
       real*8,allocatable::Z91(:,:,:,:,:,:)
       real*8,allocatable::Z92(:,:,:,:,:,:)
       real*8,allocatable::Z113(:,:,:,:,:,:)
       real*8,allocatable::Z114(:,:,:,:,:,:)
       real*8,allocatable::Z115(:,:,:,:,:,:)
       real*8,allocatable::Z116(:,:,:,:,:,:)
       real*8,allocatable::Z119(:,:,:,:,:,:)
       real*8,allocatable::Z120(:,:,:,:,:,:)
       real*8,allocatable::Z121(:,:,:,:,:,:)
       real*8,allocatable::Z122(:,:,:,:,:,:)
       real*8,allocatable::X89(:,:,:,:)
       real*8,allocatable::Z132(:,:,:,:,:,:)
       real*8,allocatable::X90(:,:,:,:)
       real*8,allocatable::Z133(:,:,:,:,:,:)
       real*8,allocatable::X91(:,:,:,:)
       real*8,allocatable::Z134(:,:,:,:,:,:)
       real*8,allocatable::X92(:,:,:,:)
       real*8,allocatable::Z135(:,:,:,:,:,:)
       real*8,allocatable::X93(:,:,:,:)
       real*8,allocatable::Z136(:,:,:,:,:,:)
       real*8,allocatable::X94(:,:,:,:)
       real*8,allocatable::Z137(:,:,:,:,:,:)
       real*8,allocatable::X95(:,:,:,:)
       real*8,allocatable::Z138(:,:,:,:,:,:)
       real*8,allocatable::X96(:,:,:,:)
       real*8,allocatable::Z139(:,:,:,:,:,:)
       real*8,allocatable::X97(:,:,:,:)
       real*8,allocatable::Z140(:,:,:,:,:,:)
       real*8,allocatable::X98(:,:,:,:)
       real*8,allocatable::Z141(:,:,:,:,:,:)
       real*8,allocatable::X99(:,:)
       real*8,allocatable::Z142(:,:,:,:,:,:)
       real*8,allocatable::X100(:,:,:,:)
       real*8,allocatable::Z143(:,:,:,:,:,:)
       real*8,allocatable::X101(:,:,:,:)
       real*8,allocatable::Z144(:,:,:,:,:,:)
       real*8,allocatable::X102(:,:,:,:)
       real*8,allocatable::Z145(:,:,:,:,:,:)
       real*8,allocatable::X103(:,:,:,:)
       real*8,allocatable::Z146(:,:,:,:,:,:)
       real*8,allocatable::X104(:,:)
       real*8,allocatable::Z147(:,:,:,:,:,:)
       real*8,allocatable::X105(:,:)
       real*8,allocatable::Z148(:,:,:,:,:,:)
       real*8,allocatable::X106(:,:)
       real*8,allocatable::Z153(:,:,:,:,:,:)
       real*8,allocatable::X107(:,:)
       real*8,allocatable::Z154(:,:,:,:,:,:)
       real*8,allocatable::X108(:,:)
       real*8,allocatable::Z158(:,:,:,:,:,:)
       real*8,allocatable::X109(:,:)
       real*8,allocatable::Z159(:,:,:,:,:,:)
       real*8,allocatable::Z160(:,:,:,:,:,:)
       real*8,allocatable::Z161(:,:,:,:,:,:)
       real*8,allocatable::Z162(:,:,:,:,:,:)
       real*8,allocatable::Z163(:,:,:,:,:,:)
       real*8,allocatable::Z164(:,:,:,:,:,:)
       real*8,allocatable::Z165(:,:,:,:,:,:)
       real*8,allocatable::Z166(:,:,:,:,:,:)
       real*8,allocatable::Z167(:,:,:,:,:,:)
       real*8,allocatable::X110(:,:,:,:)
       real*8,allocatable::Z308(:,:,:,:,:,:)
       real*8,allocatable::X111(:,:,:,:)
       real*8,allocatable::Z309(:,:,:,:,:,:)
       real*8,allocatable::X112(:,:,:,:)
       real*8,allocatable::Z310(:,:,:,:,:,:)
       real*8,allocatable::X113(:,:,:,:)
       real*8,allocatable::Z311(:,:,:,:,:,:)
       real*8,allocatable::Z314(:,:,:,:,:,:)
       real*8,allocatable::Z315(:,:,:,:,:,:)
       real*8,allocatable::X114(:,:,:,:)
       real*8,allocatable::Z316(:,:,:,:,:,:)
       real*8,allocatable::X115(:,:,:,:)
       real*8,allocatable::Z317(:,:,:,:,:,:)
       real*8,allocatable::X116(:,:,:,:)
       real*8,allocatable::Z318(:,:,:,:,:,:)
       real*8,allocatable::X117(:,:,:,:)
       real*8,allocatable::Z319(:,:,:,:,:,:)
       real*8,allocatable::Z520(:,:,:,:,:,:)
       real*8,allocatable::Z521(:,:,:,:,:,:)
       real*8,allocatable::Z525(:,:,:,:,:,:)
       real*8,allocatable::Z526(:,:,:,:,:,:)
       real*8,allocatable::Z538(:,:,:,:,:,:)
       real*8,allocatable::Z539(:,:,:,:,:,:)
       real*8,allocatable::Z540(:,:,:,:,:,:)
       real*8,allocatable::Z541(:,:,:,:,:,:)
       real*8,allocatable::Z548(:,:,:,:,:,:)
       real*8,allocatable::Z549(:,:,:,:,:,:)
       real*8,allocatable::Z550(:,:,:,:,:,:)
       real*8,allocatable::Z551(:,:,:,:,:,:)
C
       allocate(V3C(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       V3C=0.0d0
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N2,M1-N0/),'2431',VBHPPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S1(N0+1:M1,N0+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,M2+1:N3,M1+1:N2,N0+1:M1))
       X1=0.0d0
       call sum_stripe(4,shape(X1),size(X1),'4123',1.000,X1,
     & S1)
       deallocate(S1)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,M1-N0/),'2431',VBHPPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S2(N0+1:M1,N0+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N1,N2+1:M2,M1+1:N2,N0+1:M1))
       X2=0.0d0
       call sum_stripe(4,shape(X2),size(X2),'4123',1.000,X2,
     & S2)
       deallocate(S2)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N2,N0-N0/),'2431',VBHPPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S3(N0+1:M1,N0+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N1,M2+1:N3,N0+1:M1,N0+1:M1))
       X3=0.0d0
       call sum_stripe(4,shape(X3),size(X3),'4123',1.000,X3,
     & S3)
       deallocate(S3)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,N0-N0/),'2431',VBHPPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S4(N0+1:M1,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:N1,N2+1:M2,N0+1:M1,N0+1:M1))
       X4=0.0d0
       call sum_stripe(4,shape(X4),size(X4),'4123',1.000,X4,
     & S4)
       deallocate(S4)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N2,M1-N0/),'4231',VBHPPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S5(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N1+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       X5=0.0d0
       call sum_stripe(4,shape(X5),size(X5),'3124',-1.000,
     & X5,S5)
       deallocate(S5)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,M1-N0/),'4231',VBHPPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S6(N1+1:M2,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N1+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       X6=0.0d0
       call sum_stripe(4,shape(X6),size(X6),'3124',-1.000,
     & X6,S6)
       deallocate(S6)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N2,N0-N0/),'4231',VBHPPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S7(N1+1:M2,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N1+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       X7=0.0d0
       call sum_stripe(4,shape(X7),size(X7),'3124',-1.000,
     & X7,S7)
       deallocate(S7)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N0-N0/),'4231',VBHPPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S8(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N1+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       X8=0.0d0
       call sum_stripe(4,shape(X8),size(X8),'3124',-1.000,
     & X8,S8)
       deallocate(S8)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHH),size(VBHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M1-N0,N0-N0/),'4312',VBHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S9(N1+1:M2,N0+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:N2,N1+1:M2,M1+1:N2,N0+1:M1))
       X13=0.0d0
       call sum_stripe(4,shape(X13),size(X13),'2134',-1.000,
     & X13,S9)
       deallocate(S9)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHH),size(VBHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,N0-N0/),'4312',VBHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S10(N1+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(N0+1:N2,N1+1:M2,N0+1:M1,N0+1:M1))
       X14=0.0d0
       call sum_stripe(4,shape(X14),size(X14),'2134',-1.000,
     & X14,S10)
       deallocate(S10)
C
       allocate(D1(N0+1:N1,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHPHP),size(VBHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,N0-N0/),'4132',VBHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S11(N1+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       X15=0.0d0
       call sum_stripe(4,shape(X15),size(X15),'3124',-1.000,
     & X15,S11)
       deallocate(S11)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPHP),size(VBHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'4132',VBHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S12(N1+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(N2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       X16=0.0d0
       call sum_stripe(4,shape(X16),size(X16),'3124',-1.000,
     & X16,S12)
       deallocate(S12)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBPHPH),size(VBPHPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,M1-N0/),'2341',VBPHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S13(N0+1:M1,N0+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K2
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X13),size(X13),'4123',1.000,
     & X13,S13)
       deallocate(S13)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBPHPH),size(VBPHPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N0-N0/),'2341',VBPHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S14(N0+1:M1,N0+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K2
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X14),size(X14),'4123',1.000,
     & X14,S14)
       deallocate(S14)
C
       allocate(D1(N1+1:N3,N2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBAPPP),size(VBAPPP),shape(D1),
     & size(D1),(/N1-N1,N2-N2,M2-N2,N1-N1/),'2134',VBAPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S15(N0+1:M1,N2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K4
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X15),size(X15),'4123',1.000,
     & X15,S15)
       deallocate(S15)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBAPPP),size(VBAPPP),shape(D1),
     & size(D1),(/N1-N1,N2-N2,N2-N2,N1-N1/),'2134',VBAPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S16(N0+1:M1,N2+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0*K4
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X16),size(X16),'4123',1.000,
     & X16,S16)
       deallocate(S16)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(Q1(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q1)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X29(N0+1:M1,N0+1:M1))
       X29=0.0d0
       call sum_stripe(2,shape(X29),size(X29),'21',1.000,
     & X29,Q1)
       deallocate(Q1)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,M1-N0/),'12',FAHP,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(Q2(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q2)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X30(M1+1:N1,N0+1:M1))
       X30=0.0d0
       call sum_stripe(2,shape(X30),size(X30),'21',1.000,
     & X30,Q2)
       deallocate(Q2)
C
       allocate(B1(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,M2-N1/),'21',FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q3(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q3)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X31(M2+1:N3,N1+1:M2))
       X31=0.0d0
       call sum_stripe(2,shape(X31),size(X31),'21',-1.000,
     & X31,Q3)
       deallocate(Q3)
C
       allocate(B1(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,N1-N1/),'21',FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q4(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q4)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X32(N1+1:M2,N1+1:M2))
       X32=0.0d0
       call sum_stripe(2,shape(X32),size(X32),'21',-1.000,
     & X32,Q4)
       deallocate(Q4)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N0-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S17(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S17),size(S17),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N0-N0/),'2314',S17,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z113(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z113)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z113)
       deallocate(Z113)
       deallocate(S17)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N0-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S18(N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S18),size(S18),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,N0-N0/),'2314',S18,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z114(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z114)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z114)
       deallocate(Z114)
       deallocate(S18)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S19(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S19),size(S19),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2314',S19,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z115(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z115)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z115)
       deallocate(Z115)
       deallocate(S19)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N0-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S20(N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S20),size(S20),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N1-N1,N0-N0/),'2314',S20,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z116(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z116)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z116)
       deallocate(Z116)
       deallocate(S20)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'3142',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q5(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       X29=X29+Q5
       deallocate(Q5)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N0-N0/),'3142',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q6(M1+1:N1,N0+1:M1))
       I1=K5*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       X30=X30+Q6
       deallocate(Q6)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N1,N1-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S21(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S21),size(S21),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N0-N0/),'2341',S21,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z119(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z119)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z119)
       deallocate(Z119)
       deallocate(S21)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N1,N1-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S22(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S22),size(S22),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,N0-N0/),'2341',S22,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z120(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z120)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z120)
       deallocate(Z120)
       deallocate(S22)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N1-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S23(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S23),size(S23),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2341',S23,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z121(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z121)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z121)
       deallocate(Z121)
       deallocate(S23)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N1-N1,N1-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S24(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S24),size(S24),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N1-N1,N0-N0/),'2341',S24,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z122(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z122)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z122)
       deallocate(Z122)
       deallocate(S24)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q7(M2+1:N3,N1+1:M2))
       I1=K9*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       X31=X31-Q7
       deallocate(Q7)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q8(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       X32=X32-Q8
       deallocate(Q8)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,M1-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S25(N0+1:M1,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X44(M1+1:N2,N0+1:M1,M1+1:N2,N0+1:M1))
       X44=0.0d0
       call sum_stripe(4,shape(X44),size(X44),'4123',1.000,
     & X44,S25)
       deallocate(S25)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,M1-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S26(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N2))
       I1=K8*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X45(N0+1:M1,M1+1:N1,M1+1:N2,N0+1:M1))
       X45=0.0d0
       call sum_stripe(4,shape(X45),size(X45),'4123',1.000,
     & X45,S26)
       deallocate(S26)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,M1-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S27(N0+1:M1,M1+1:N2,M1+1:N1,M1+1:N2))
       I1=K8*K7*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X46(M1+1:N2,M1+1:N1,M1+1:N2,N0+1:M1))
       X46=0.0d0
       call sum_stripe(4,shape(X46),size(X46),'4123',1.000,
     & X46,S27)
       deallocate(S27)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N0-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S28(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X47(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       X47=0.0d0
       call sum_stripe(4,shape(X47),size(X47),'4123',1.000,
     & X47,S28)
       deallocate(S28)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N0-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S29(N0+1:M1,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X48(M1+1:N2,N0+1:M1,N0+1:M1,N0+1:M1))
       X48=0.0d0
       call sum_stripe(4,shape(X48),size(X48),'4123',1.000,
     & X48,S29)
       deallocate(S29)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N0-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S30(N0+1:M1,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X49(N0+1:M1,M1+1:N1,N0+1:M1,N0+1:M1))
       X49=0.0d0
       call sum_stripe(4,shape(X49),size(X49),'4123',1.000,
     & X49,S30)
       deallocate(S30)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,N0-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S31(N0+1:M1,M1+1:N2,M1+1:N1,N0+1:M1))
       I1=K5*K7*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X50(M1+1:N2,M1+1:N1,N0+1:M1,N0+1:M1))
       X50=0.0d0
       call sum_stripe(4,shape(X50),size(X50),'4123',1.000,
     & X50,S31)
       deallocate(S31)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N2,M2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S32(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X89(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X89=0.0d0
       call sum_stripe(4,shape(X89),size(X89),'4123',1.000,
     & X89,S32)
       deallocate(S32)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N2,M2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S33(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X90(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       X90=0.0d0
       call sum_stripe(4,shape(X90),size(X90),'4123',1.000,
     & X90,S33)
       deallocate(S33)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,M2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S34(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X91(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       X91=0.0d0
       call sum_stripe(4,shape(X91),size(X91),'4123',1.000,
     & X91,S34)
       deallocate(S34)
C
       allocate(D1(N1+1:N3,M1+1:N1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,M2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S35(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X92(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:M1))
       X92=0.0d0
       call sum_stripe(4,shape(X92),size(X92),'4123',1.000,
     & X92,S35)
       deallocate(S35)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N2,N2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S36(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X93(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X93=0.0d0
       call sum_stripe(4,shape(X93),size(X93),'4123',1.000,
     & X93,S36)
       deallocate(S36)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N2,N2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S37(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X94(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X94=0.0d0
       call sum_stripe(4,shape(X94),size(X94),'4123',1.000,
     & X94,S37)
       deallocate(S37)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,N2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S38(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X95(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X95=0.0d0
       call sum_stripe(4,shape(X95),size(X95),'4123',1.000,
     & X95,S38)
       deallocate(S38)
C
       allocate(D1(N1+1:N3,M1+1:N1,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,N2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S39(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X96(M1+1:N1,N2+1:M2,N2+1:M2,N0+1:M1))
       X96=0.0d0
       call sum_stripe(4,shape(X96),size(X96),'4123',1.000,
     & X96,S39)
       deallocate(S39)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S40(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X97(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N2))
       X97=0.0d0
       call sum_stripe(4,shape(X97),size(X97),'3124',1.000,
     & X97,S40)
       deallocate(S40)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S41(N1+1:M2,M1+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X98(M1+1:N2,N1+1:M2,N1+1:M2,M1+1:N2))
       X98=0.0d0
       call sum_stripe(4,shape(X98),size(X98),'3124',1.000,
     & X98,S41)
       deallocate(S41)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q9(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X99(M1+1:N2,M1+1:N2))
       X99=0.0d0
       X99=X99+Q9
       deallocate(Q9)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N0-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S42(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X100(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X100=0.0d0
       call sum_stripe(4,shape(X100),size(X100),'3124',
     & 1.000,X100,S42)
       deallocate(S42)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N0-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S43(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X101(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       X101=0.0d0
       call sum_stripe(4,shape(X101),size(X101),'3124',
     & 1.000,X101,S43)
       deallocate(S43)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S44(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X102(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X102=0.0d0
       call sum_stripe(4,shape(X102),size(X102),'3124',
     & 1.000,X102,S44)
       deallocate(S44)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N0-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S45(N1+1:M2,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X103(M1+1:N2,N1+1:M2,N1+1:M2,N0+1:M1))
       X103=0.0d0
       call sum_stripe(4,shape(X103),size(X103),'3124',
     & 1.000,X103,S45)
       deallocate(S45)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q10(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X104(N0+1:M1,N0+1:M1))
       X104=0.0d0
       X104=X104+Q10
       deallocate(Q10)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q11(M1+1:N2,N0+1:M1))
       I1=K5*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X105(M1+1:N2,N0+1:M1))
       X105=0.0d0
       X105=X105+Q11
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,M2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S46(N1+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       I1=K6*K6*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X65(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       X65=0.0d0
       call sum_stripe(4,shape(X65),size(X65),'4123',-1.000,
     & X65,S46)
       deallocate(S46)
C
       allocate(D1(N0+1:N1,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,M2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S47(N1+1:M2,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K0
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X66(N2+1:M2,M2+1:N3,M2+1:N3,N1+1:M2))
       X66=0.0d0
       call sum_stripe(4,shape(X66),size(X66),'4123',-1.000,
     & X66,S47)
       deallocate(S47)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,M2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S48(N1+1:M2,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X67(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       X67=0.0d0
       call sum_stripe(4,shape(X67),size(X67),'4123',-1.000,
     & X67,S48)
       deallocate(S48)
C
       allocate(D1(N0+1:N1,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S49(N1+1:M2,N2+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K0
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X68(N2+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       X68=0.0d0
       call sum_stripe(4,shape(X68),size(X68),'4123',-1.000,
     & X68,S49)
       deallocate(S49)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N2,M2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q12(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X106(M2+1:N3,M2+1:N3))
       X106=0.0d0
       X106=X106+Q12
       deallocate(Q12)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,M2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q13(N2+1:M2,M2+1:N3))
       I1=K6*K0
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X107(N2+1:M2,M2+1:N3))
       X107=0.0d0
       X107=X107+Q13
       deallocate(Q13)
C
       allocate(D1(N0+1:N1,N2+1:M2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S50(N1+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K0
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X69(N2+1:M2,M2+1:N3,N2+1:M2,N1+1:M2))
       X69=0.0d0
       call sum_stripe(4,shape(X69),size(X69),'4123',-1.000,
     & X69,S50)
       deallocate(S50)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S51(N1+1:M2,M2+1:N3,N1+1:M2,N2+1:M2))
       I1=K0*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X70(M2+1:N3,N1+1:M2,N2+1:M2,N1+1:M2))
       X70=0.0d0
       call sum_stripe(4,shape(X70),size(X70),'4123',-1.000,
     & X70,S51)
       deallocate(S51)
C
       allocate(D1(N0+1:N1,N2+1:M2,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S52(N1+1:M2,N2+1:M2,N1+1:M2,N2+1:M2))
       I1=K0*K9*K0
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X71(N2+1:M2,N1+1:M2,N2+1:M2,N1+1:M2))
       X71=0.0d0
       call sum_stripe(4,shape(X71),size(X71),'4123',-1.000,
     & X71,S52)
       deallocate(S52)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q14(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X108(M2+1:N3,N2+1:M2))
       X108=0.0d0
       X108=X108+Q14
       deallocate(Q14)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q15(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X109(N2+1:M2,N2+1:M2))
       X109=0.0d0
       X109=X109+Q15
       deallocate(Q15)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S53(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S53),size(S53),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N0-N0/),'2314',S53,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z160(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z160)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z160)
       deallocate(Z160)
       deallocate(S53)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S54(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S54),size(S54),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,N0-N0/),'2314',S54,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z161(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z161)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z161)
       deallocate(Z161)
       deallocate(S54)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S55(N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S55),size(S55),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'2314',S55,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'421356',t3D,F2)
       allocate(Z162(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z162)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z162)
       deallocate(Z162)
       deallocate(S55)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S56(N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S56),size(S56),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,N0-N0/),'2314',S56,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'521346',t3D,F2)
       allocate(Z163(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z163)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z163)
       deallocate(Z163)
       deallocate(S56)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S57(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S57),size(S57),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N0-N0/),'2341',S57,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z164(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z164)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z164)
       deallocate(Z164)
       deallocate(S57)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S58(N0+1:M1,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S58),size(S58),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,N0-N0/),'2341',S58,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z165(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z165)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z165)
       deallocate(Z165)
       deallocate(S58)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S59(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S59),size(S59),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'2341',S59,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'421356',t3D,F2)
       allocate(Z166(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z166)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z166)
       deallocate(Z166)
       deallocate(S59)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S60(N0+1:M1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S60),size(S60),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,N0-N0/),'2341',S60,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'521346',t3D,F2)
       allocate(Z167(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z167)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z167)
       deallocate(Z167)
       deallocate(S60)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHH),size(VBHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M1-N0,N0-N0/),'3412',VBHHHH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S61(M2+1:N3,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S61)
       deallocate(S61)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHH),size(VBHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M1-N0,N0-N0/),'3412',VBHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S62(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S62)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S62)
       deallocate(S62)
C
       allocate(D1(N2+1:N3,N0+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHPHP),size(VBHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,N0-N0/),'1432',VBHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S63(M1+1:N2,N0+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'3124',1.000,X1,
     & S63)
       deallocate(S63)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPHP),size(VBHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N0-N0/),'1432',VBHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S64(M1+1:N2,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'3124',1.000,X2,
     & S64)
       deallocate(S64)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHH),size(VBHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,N0-N0/),'3412',VBHHHH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S65(M2+1:N3,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',-1.000,
     & X3,S65)
       deallocate(S65)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHH),size(VBHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,N0-N0/),'3412',VBHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S66(N2+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S66)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',-1.000,
     & X4,S66)
       deallocate(S66)
C
       allocate(D1(N2+1:N3,N0+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHPHP),size(VBHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,N0-N0/),'1432',VBHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S67(N0+1:M1,N0+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K1
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'3124',1.000,X3,
     & S67)
       deallocate(S67)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPHP),size(VBHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N0-N0/),'1432',VBHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S68(N0+1:M1,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S68)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'3124',1.000,X4,
     & S68)
       deallocate(S68)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBPHPH),size(VBPHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M1-N0/),'3241',VBPHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S69(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',-1.000,
     & X5,S69)
       deallocate(S69)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBPHPH),size(VBPHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M1-N0/),'3241',VBPHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S70(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S70)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'2134',-1.000,
     & X6,S70)
       deallocate(S70)
C
       allocate(D1(N2+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBAPPP),size(VBAPPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,M2-N2,N1-N1/),'1234',VBAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S71(M1+1:N2,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'4123',1.000,X5,
     & S71)
       deallocate(S71)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBAPPP),size(VBAPPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N2-N2,N1-N1/),'1234',VBAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S72(M1+1:N2,N1+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0*K3
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S72)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'4123',1.000,X6,
     & S72)
       deallocate(S72)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBPHPH),size(VBPHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3241',VBPHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S73(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',-1.000,
     & X7,S73)
       deallocate(S73)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBPHPH),size(VBPHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3241',VBPHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S74(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S74)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',-1.000,
     & X8,S74)
       deallocate(S74)
C
       allocate(D1(N2+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBAPPP),size(VBAPPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,M2-N2,N1-N1/),'1234',VBAPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S75(N0+1:M1,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'4123',1.000,X7,
     & S75)
       deallocate(S75)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBAPPP),size(VBAPPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N2-N2,N1-N1/),'1234',VBAPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S76(N0+1:M1,N1+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0*K3
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S76)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'4123',1.000,X8,
     & S76)
       deallocate(S76)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VCHHHH),size(VCHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,M1-N0/),'3412',VCHHHH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S77(M2+1:N3,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:N2,M2+1:N3,N0+1:M1,M1+1:N2))
       X9=0.0d0
       call sum_stripe(4,shape(X9),size(X9),'2134',-1.000,
     & X9,S77)
       deallocate(S77)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VCHHHH),size(VCHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,M1-N0/),'3412',VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S78(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S78)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       X10=0.0d0
       call sum_stripe(4,shape(X10),size(X10),'2134',-1.000,
     & X10,S78)
       deallocate(S78)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,M1-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S79(N0+1:M1,N0+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'3124',1.000,X9,
     & S79)
       deallocate(S79)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,M1-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S80(N0+1:M1,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S80)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'3124',1.000,
     & X10,S80)
       deallocate(S80)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S81(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S81)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X11=0.0d0
       call sum_stripe(4,shape(X11),size(X11),'3124',-1.000,
     & X11,S81)
       deallocate(S81)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S82(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S82)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'2134',1.000,
     & X11,S82)
       deallocate(S82)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,N0-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S83(M1+1:N2,N0+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'4123',-1.000,
     & X9,S83)
       deallocate(S83)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N0-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S84(M1+1:N2,N0+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S84)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'4123',-1.000,
     & X10,S84)
       deallocate(S84)
C
       allocate(D1(N2+1:N3,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCAPPP),size(VCAPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,M2-N2,N2-N2/),'1234',VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S85(M1+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K4
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S85)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'4123',-1.000,
     & X11,S85)
       deallocate(S85)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,N0-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S86(N2+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S86)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(N2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       X12=0.0d0
       call sum_stripe(4,shape(X12),size(X12),'3124',-1.000,
     & X12,S86)
       deallocate(S86)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S87(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S87)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'2134',1.000,
     & X12,S87)
       deallocate(S87)
C
       allocate(D1(N2+1:N3,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCAPPP),size(VCAPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,M2-N2,N2-N2/),'1234',VCAPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S88(N0+1:M1,N2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K4
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S88)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'4123',-1.000,
     & X12,S88)
       deallocate(S88)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N0-N0/),'3124',VBHPPH,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S89(M1+1:N2,N0+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S89)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X13),size(X13),'3124',1.000,
     & X13,S89)
       deallocate(S89)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N0-N0/),'3124',VBHPPH,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S90(N0+1:M1,N0+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X14),size(X14),'3124',1.000,
     & X14,S90)
       deallocate(S90)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'1324',VBHPPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S91(M2+1:N3,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X15),size(X15),'2134',-1.000,
     & X15,S91)
       deallocate(S91)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'1324',VBHPPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S92(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S92)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X16),size(X16),'2134',-1.000,
     & X16,S92)
       deallocate(S92)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S93(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N2))
       X17=0.0d0
       call sum_stripe(4,shape(X17),size(X17),'3124',-1.000,
     & X17,S93)
       deallocate(S93)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S94(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S94)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X18(M1+1:N1,N1+1:M2,M2+1:N3,M1+1:N2))
       X18=0.0d0
       call sum_stripe(4,shape(X18),size(X18),'3124',-1.000,
     & X18,S94)
       deallocate(S94)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S95(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X19(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X19=0.0d0
       call sum_stripe(4,shape(X19),size(X19),'3124',-1.000,
     & X19,S95)
       deallocate(S95)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S96(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S96)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X20(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X20=0.0d0
       call sum_stripe(4,shape(X20),size(X20),'3124',-1.000,
     & X20,S96)
       deallocate(S96)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S97(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X17),size(X17),'4123',1.000,
     & X17,S97)
       deallocate(S97)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S98(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S98)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X18),size(X18),'4123',1.000,
     & X18,S98)
       deallocate(S98)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S99(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X19),size(X19),'4123',1.000,
     & X19,S99)
       deallocate(S99)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S100(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S100)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X20),size(X20),'4123',1.000,
     & X20,S100)
       deallocate(S100)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S101(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X21(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X21=0.0d0
       call sum_stripe(4,shape(X21),size(X21),'3124',-1.000,
     & X21,S101)
       deallocate(S101)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S102(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S102)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X22(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       X22=0.0d0
       call sum_stripe(4,shape(X22),size(X22),'3124',-1.000,
     & X22,S102)
       deallocate(S102)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S103(M2+1:N3,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X23(N0+1:M1,N1+1:M2,M2+1:N3,N0+1:M1))
       X23=0.0d0
       call sum_stripe(4,shape(X23),size(X23),'3124',-1.000,
     & X23,S103)
       deallocate(S103)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S104(M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S104)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X24(M1+1:N1,N1+1:M2,M2+1:N3,N0+1:M1))
       X24=0.0d0
       call sum_stripe(4,shape(X24),size(X24),'3124',-1.000,
     & X24,S104)
       deallocate(S104)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S105(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X25(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X25=0.0d0
       call sum_stripe(4,shape(X25),size(X25),'3124',-1.000,
     & X25,S105)
       deallocate(S105)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S106(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S106)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X26(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X26=0.0d0
       call sum_stripe(4,shape(X26),size(X26),'3124',-1.000,
     & X26,S106)
       deallocate(S106)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S107(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S107)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X27(N0+1:M1,N1+1:M2,N2+1:M2,N0+1:M1))
       X27=0.0d0
       call sum_stripe(4,shape(X27),size(X27),'3124',-1.000,
     & X27,S107)
       deallocate(S107)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S108(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S108)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X28(M1+1:N1,N1+1:M2,N2+1:M2,N0+1:M1))
       X28=0.0d0
       call sum_stripe(4,shape(X28),size(X28),'3124',-1.000,
     & X28,S108)
       deallocate(S108)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S109(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S109)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X21),size(X21),'4123',1.000,
     & X21,S109)
       deallocate(S109)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S110(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S110)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X22),size(X22),'4123',1.000,
     & X22,S110)
       deallocate(S110)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S111(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S111)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X23),size(X23),'4123',1.000,
     & X23,S111)
       deallocate(S111)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S112(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S112)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X24),size(X24),'4123',1.000,
     & X24,S112)
       deallocate(S112)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S113(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S113)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X25),size(X25),'4123',1.000,
     & X25,S113)
       deallocate(S113)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S114(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S114)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X26),size(X26),'4123',1.000,
     & X26,S114)
       deallocate(S114)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S115(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S115)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X27),size(X27),'4123',1.000,
     & X27,S115)
       deallocate(S115)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S116(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S116)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X28),size(X28),'4123',1.000,
     & X28,S116)
       deallocate(S116)
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
       allocate(X33(M1+1:N2,M1+1:N2))
       X33=0.0d0
       call sum_stripe(2,shape(X33),size(X33),'21',1.000,
     & X33,Q16)
       deallocate(Q16)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q17(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q17)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X34(N0+1:M1,N0+1:M1))
       X34=0.0d0
       call sum_stripe(2,shape(X34),size(X34),'21',1.000,
     & X34,Q17)
       deallocate(Q17)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,M1-N0/),'12',FBHP,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q18(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q18)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X35(M1+1:N2,N0+1:M1))
       X35=0.0d0
       call sum_stripe(2,shape(X35),size(X35),'21',1.000,
     & X35,Q18)
       deallocate(Q18)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,M2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q19(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q19)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X36(M2+1:N3,M2+1:N3))
       X36=0.0d0
       call sum_stripe(2,shape(X36),size(X36),'21',-1.000,
     & X36,Q19)
       deallocate(Q19)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,N2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q20(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q20)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X37(N2+1:M2,M2+1:N3))
       X37=0.0d0
       call sum_stripe(2,shape(X37),size(X37),'21',-1.000,
     & X37,Q20)
       deallocate(Q20)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,M2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q21(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q21)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X38(M2+1:N3,N2+1:M2))
       X38=0.0d0
       call sum_stripe(2,shape(X38),size(X38),'21',-1.000,
     & X38,Q21)
       deallocate(Q21)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,N2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q22(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q22)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X39(N2+1:M2,N2+1:M2))
       X39=0.0d0
       call sum_stripe(2,shape(X39),size(X39),'21',-1.000,
     & X39,Q22)
       deallocate(Q22)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,N0-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S117(M1+1:N2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S117)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X44),size(X44),'3124',1.000,
     & X44,S117)
       deallocate(S117)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N0-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S118(M1+1:N2,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S118)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X45),size(X45),'3124',1.000,
     & X45,S118)
       deallocate(S118)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,N0-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S119(M1+1:N2,M1+1:N2,M1+1:N1,N0+1:M1))
       I1=K5*K7*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S119)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X46),size(X46),'3124',1.000,
     & X46,S119)
       deallocate(S119)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N0-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S120(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S120)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X47),size(X47),'3124',1.000,
     & X47,S120)
       deallocate(S120)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,N0-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S121(N0+1:M1,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S121)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X48),size(X48),'3124',1.000,
     & X48,S121)
       deallocate(S121)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N0-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S122(N0+1:M1,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S122)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X49),size(X49),'3124',1.000,
     & X49,S122)
       deallocate(S122)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,N0-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S123(N0+1:M1,M1+1:N2,M1+1:N1,N0+1:M1))
       I1=K5*K7*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S123)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X50),size(X50),'3124',1.000,
     & X50,S123)
       deallocate(S123)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S124(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S124)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X51(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X51=0.0d0
       call sum_stripe(4,shape(X51),size(X51),'3124',-1.000,
     & X51,S124)
       deallocate(S124)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,M1,X51,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'612345',t3C1,F2)
       allocate(Z51(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X51,F2,Z51)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234516',
     & -1.000,V3C,Z51)
       deallocate(Z51)
       deallocate(X51)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S125(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S125)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X52(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       X52=0.0d0
       call sum_stripe(4,shape(X52),size(X52),'3124',-1.000,
     & X52,S125)
       deallocate(S125)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,M1,X52,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'612345',t3C1,F2)
       allocate(Z52(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K9*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X52,F2,Z52)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234516',
     & -1.000,V3C,Z52)
       deallocate(Z52)
       deallocate(X52)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S126(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S126)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X53(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       X53=0.0d0
       call sum_stripe(4,shape(X53),size(X53),'3124',-1.000,
     & X53,S126)
       deallocate(S126)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,M1,X53,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'612345',t3C1,F2)
       allocate(Z53(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K9*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,X53,F2,Z53)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234516',
     & -1.000,V3C,Z53)
       deallocate(Z53)
       deallocate(X53)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S127(M2+1:N3,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S127)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X54(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:M1))
       X54=0.0d0
       call sum_stripe(4,shape(X54),size(X54),'3124',-1.000,
     & X54,S127)
       deallocate(S127)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,M1,X54,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'612345',t3C1,F2)
       allocate(Z54(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K9*K0
       I3=K0*K7
       call EGEMM(I1,I2,I3,X54,F2,Z54)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234516',
     & -1.000,V3C,Z54)
       deallocate(Z54)
       deallocate(X54)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S128(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S128)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X55(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X55=0.0d0
       call sum_stripe(4,shape(X55),size(X55),'3124',-1.000,
     & X55,S128)
       deallocate(S128)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,N0,M1,X55,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,M2-M2,N1-N1,N0-N0,M1-M1/),'612345',t3C3,F2)
       allocate(Z55(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X55,F2,Z55)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134526',
     & 1.000,V3C,Z55)
       deallocate(Z55)
       deallocate(X55)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S129(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S129)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X56(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X56=0.0d0
       call sum_stripe(4,shape(X56),size(X56),'3124',-1.000,
     & X56,S129)
       deallocate(S129)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,N0,M1,X56,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,M2-M2,N1-N1,N0-N0,M1-M1/),'612345',t3C3,F2)
       allocate(Z56(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X56,F2,Z56)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134526',
     & 1.000,V3C,Z56)
       deallocate(Z56)
       deallocate(X56)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S130(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S130)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X57(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X57=0.0d0
       call sum_stripe(4,shape(X57),size(X57),'3124',-1.000,
     & X57,S130)
       deallocate(S130)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,N2,M2,N2,M2,N0,M1,X57,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N1-N1,N0-N0,M1-M1/),'621345',t3C1,F2)
       allocate(Z57(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X57,F2,Z57)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134526',
     & -1.000,V3C,Z57)
       deallocate(Z57)
       deallocate(X57)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S131(N2+1:M2,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S131)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X58(M1+1:N1,N2+1:M2,N2+1:M2,N0+1:M1))
       X58=0.0d0
       call sum_stripe(4,shape(X58),size(X58),'3124',-1.000,
     & X58,S131)
       deallocate(S131)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,N2,M2,N2,M2,N0,M1,X58,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N1-N1,N0-N0,M1-M1/),'621345',t3C1,F2)
       allocate(Z58(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K6
       I3=K0*K7
       call EGEMM(I1,I2,I3,X58,F2,Z58)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134526',
     & -1.000,V3C,Z58)
       deallocate(Z58)
       deallocate(X58)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q23(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q23)
       deallocate(D1)
       deallocate(B2)
C
       X29=X29+Q23
       deallocate(Q23)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N0-N0/),'3142',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q24(M1+1:N1,N0+1:M1))
       I1=K5*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q24)
       deallocate(D1)
       deallocate(B2)
C
       X30=X30+Q24
       deallocate(Q24)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S132(M1+1:N2,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S132)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X59(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N2))
       X59=0.0d0
       call sum_stripe(4,shape(X59),size(X59),'4123',1.000,
     & X59,S132)
       deallocate(S132)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,M1,N2,X59,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N2,N2-N2,N0-N0,N0-N0/),'531246',t3C1,F2)
       allocate(Z59(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K5*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X59,F2,Z59)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124635',
     & -1.000,V3C,Z59)
       deallocate(Z59)
       deallocate(X59)
C
       allocate(D1(N2+1:N3,M1+1:N2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S133(M1+1:N2,M1+1:N2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S133)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X60(M1+1:N2,N1+1:M2,N1+1:M2,M1+1:N2))
       X60=0.0d0
       call sum_stripe(4,shape(X60),size(X60),'4123',1.000,
     & X60,S133)
       deallocate(S133)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,N1,M2,N1,M2,M1,N2,X60,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N1-N1,M2-N2,N2-N2,N0-N0,N0-N0/),'531246',t3C1,F2)
       allocate(Z60(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K5*K5*K0*K6
       I3=K9*K8
       call EGEMM(I1,I2,I3,X60,F2,Z60)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124635',
     & -1.000,V3C,Z60)
       deallocate(Z60)
       deallocate(X60)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S134(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S134)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X61(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X61=0.0d0
       call sum_stripe(4,shape(X61),size(X61),'4123',1.000,
     & X61,S134)
       deallocate(S134)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,N1,M2,N0,M1,X61,VBPHPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N2,N2-N2,M1-M1,N0-N0/),'431256',t3C1,F2)
       allocate(Z61(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X61,F2,Z61)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & -1.000,V3C,Z61)
       deallocate(Z61)
       deallocate(X61)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S135(N0+1:M1,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S135)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X62(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       X62=0.0d0
       call sum_stripe(4,shape(X62),size(X62),'4123',1.000,
     & X62,S135)
       deallocate(S135)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,N0,M1,X62,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N2,N2-N2,M1-M1,N0-N0/),'431256',t3C1,F2)
       allocate(Z62(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X62,F2,Z62)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & -1.000,V3C,Z62)
       deallocate(Z62)
       deallocate(X62)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S136(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S136)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X63(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X63=0.0d0
       call sum_stripe(4,shape(X63),size(X63),'4123',1.000,
     & X63,S136)
       deallocate(S136)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,M1,N1,M2,N1,M2,N0,M1,X63,VBPHPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,N2-N2,M1-M1,N0-N0/),'431256',t3C1,F2)
       allocate(Z63(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X63,F2,Z63)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & -1.000,V3C,Z63)
       deallocate(Z63)
       deallocate(X63)
C
       allocate(D1(N2+1:N3,M1+1:N2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S137(N0+1:M1,M1+1:N2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S137)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X64(M1+1:N2,N1+1:M2,N1+1:M2,N0+1:M1))
       X64=0.0d0
       call sum_stripe(4,shape(X64),size(X64),'4123',1.000,
     & X64,S137)
       deallocate(S137)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,N1,M2,N1,M2,N0,M1,X64,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,N2-N2,M1-M1,N0-N0/),'431256',t3C1,F2)
       allocate(Z64(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K6
       I3=K9*K8
       call EGEMM(I1,I2,I3,X64,F2,Z64)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & -1.000,V3C,Z64)
       deallocate(Z64)
       deallocate(X64)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S138(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S138)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X65),size(X65),'3124',-1.000,
     & X65,S138)
       deallocate(S138)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S139(M2+1:N3,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S139)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X66),size(X66),'3124',-1.000,
     & X66,S139)
       deallocate(S139)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S140(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S140)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X67),size(X67),'3124',-1.000,
     & X67,S140)
       deallocate(S140)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S141(M2+1:N3,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S141)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X68),size(X68),'3124',-1.000,
     & X68,S141)
       deallocate(S141)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S142(N2+1:M2,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S142)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X69),size(X69),'3124',-1.000,
     & X69,S142)
       deallocate(S142)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S143(N2+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S143)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X70),size(X70),'3124',-1.000,
     & X70,S143)
       deallocate(S143)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S144(N2+1:M2,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S144)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X71),size(X71),'3124',-1.000,
     & X71,S144)
       deallocate(S144)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q25(M2+1:N3,N1+1:M2))
       I1=K9*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q25)
       deallocate(D1)
       deallocate(B2)
C
       X31=X31+Q25
       deallocate(Q25)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q26(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q26)
       deallocate(D1)
       deallocate(B2)
C
       X32=X32+Q26
       deallocate(Q26)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M1-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S145(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S145)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X72(N0+1:M1,M1+1:N2,N0+1:M1,M1+1:N2))
       X72=0.0d0
       call sum_stripe(4,shape(X72),size(X72),'3124',1.000,
     & X72,S145)
       deallocate(S145)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,M1-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S146(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S146)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X73(M1+1:N2,M1+1:N2,N0+1:M1,M1+1:N2))
       X73=0.0d0
       call sum_stripe(4,shape(X73),size(X73),'3124',1.000,
     & X73,S146)
       deallocate(S146)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S147(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S147)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X74(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       X74=0.0d0
       call sum_stripe(4,shape(X74),size(X74),'3124',-1.000,
     & X74,S147)
       deallocate(S147)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S148(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S148)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X75(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N2))
       X75=0.0d0
       call sum_stripe(4,shape(X75),size(X75),'3124',-1.000,
     & X75,S148)
       deallocate(S148)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S149(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S149)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X76(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X76=0.0d0
       call sum_stripe(4,shape(X76),size(X76),'3124',-1.000,
     & X76,S149)
       deallocate(S149)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S150(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S150)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X77(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X77=0.0d0
       call sum_stripe(4,shape(X77),size(X77),'3124',-1.000,
     & X77,S150)
       deallocate(S150)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M1-N0/),'3142',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q27(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q27)
       deallocate(D1)
       deallocate(B2)
C
       X33=X33+Q27
       deallocate(Q27)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N0-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S151(M1+1:N2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S151)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X72),size(X72),'4123',-1.000,
     & X72,S151)
       deallocate(S151)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,N0-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S152(M1+1:N2,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S152)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X73),size(X73),'4123',-1.000,
     & X73,S152)
       deallocate(S152)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S153(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S153)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X74),size(X74),'4123',-1.000,
     & X74,S153)
       deallocate(S153)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S154(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S154)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X75),size(X75),'4123',-1.000,
     & X75,S154)
       deallocate(S154)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S155(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S155)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X76),size(X76),'4123',-1.000,
     & X76,S155)
       deallocate(S155)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S156(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S156)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X77),size(X77),'4123',-1.000,
     & X77,S156)
       deallocate(S156)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S157(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S157)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X78(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X78=0.0d0
       call sum_stripe(4,shape(X78),size(X78),'3124',-1.000,
     & X78,S157)
       deallocate(S157)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S158(M2+1:N3,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S158)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X79(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       X79=0.0d0
       call sum_stripe(4,shape(X79),size(X79),'3124',-1.000,
     & X79,S158)
       deallocate(S158)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S159(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S159)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X80(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       X80=0.0d0
       call sum_stripe(4,shape(X80),size(X80),'3124',-1.000,
     & X80,S159)
       deallocate(S159)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S160(M2+1:N3,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S160)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X81(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       X81=0.0d0
       call sum_stripe(4,shape(X81),size(X81),'3124',-1.000,
     & X81,S160)
       deallocate(S160)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S161(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S161)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X82(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X82=0.0d0
       call sum_stripe(4,shape(X82),size(X82),'3124',-1.000,
     & X82,S161)
       deallocate(S161)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S162(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S162)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X83(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       X83=0.0d0
       call sum_stripe(4,shape(X83),size(X83),'3124',-1.000,
     & X83,S162)
       deallocate(S162)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S163(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S163)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X84(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X84=0.0d0
       call sum_stripe(4,shape(X84),size(X84),'3124',-1.000,
     & X84,S163)
       deallocate(S163)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S164(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S164)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X85(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       X85=0.0d0
       call sum_stripe(4,shape(X85),size(X85),'3124',-1.000,
     & X85,S164)
       deallocate(S164)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q28(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q28)
       deallocate(D1)
       deallocate(B2)
C
       X34=X34+Q28
       deallocate(Q28)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N0-N0/),'3142',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q29(M1+1:N2,N0+1:M1))
       I1=K5*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q29)
       deallocate(D1)
       deallocate(B2)
C
       X35=X35+Q29
       deallocate(Q29)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S165(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S165)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X78),size(X78),'4123',-1.000,
     & X78,S165)
       deallocate(S165)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S166(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S166)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X79),size(X79),'4123',-1.000,
     & X79,S166)
       deallocate(S166)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S167(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S167)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X80),size(X80),'4123',-1.000,
     & X80,S167)
       deallocate(S167)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S168(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S168)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X81),size(X81),'4123',-1.000,
     & X81,S168)
       deallocate(S168)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S169(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S169)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X82),size(X82),'4123',-1.000,
     & X82,S169)
       deallocate(S169)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S170(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S170)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X83),size(X83),'4123',-1.000,
     & X83,S170)
       deallocate(S170)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S171(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S171)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X84),size(X84),'4123',-1.000,
     & X84,S171)
       deallocate(S171)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S172(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S172)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X85),size(X85),'4123',-1.000,
     & X85,S172)
       deallocate(S172)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S173(N2+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       I1=K6*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S173)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X86(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       X86=0.0d0
       call sum_stripe(4,shape(X86),size(X86),'4123',-1.000,
     & X86,S173)
       deallocate(S173)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S174(N2+1:M2,M2+1:N3,N2+1:M2,M2+1:N3))
       I1=K6*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S174)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X87(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       X87=0.0d0
       call sum_stripe(4,shape(X87),size(X87),'4123',-1.000,
     & X87,S174)
       deallocate(S174)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S175(N2+1:M2,N2+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S175)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X88(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       X88=0.0d0
       call sum_stripe(4,shape(X88),size(X88),'4123',-1.000,
     & X88,S175)
       deallocate(S175)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q30(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q30)
       deallocate(D1)
       deallocate(B2)
C
       X36=X36-Q30
       deallocate(Q30)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q31(N2+1:M2,M2+1:N3))
       I1=K6*K0
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q31)
       deallocate(D1)
       deallocate(B2)
C
       X37=X37-Q31
       deallocate(Q31)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S176(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S176)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X86),size(X86),'3124',1.000,
     & X86,S176)
       deallocate(S176)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S177(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S177)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X87),size(X87),'3124',1.000,
     & X87,S177)
       deallocate(S177)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S178(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S178)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X88),size(X88),'3124',1.000,
     & X88,S178)
       deallocate(S178)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q32(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q32)
       deallocate(D1)
       deallocate(B2)
C
       X38=X38-Q32
       deallocate(Q32)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q33(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q33)
       deallocate(D1)
       deallocate(B2)
C
       X39=X39-Q33
       deallocate(Q33)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S179(N1+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S179)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'2413',1.000,
     & X13,S179)
       deallocate(S179)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S180(N1+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S180)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'2413',1.000,
     & X14,S180)
       deallocate(S180)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,M2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S181(N1+1:M2,N0+1:M1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S181)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X15),size(X15),'3412',1.000,
     & X15,S181)
       deallocate(S181)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S182(N1+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S182)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X16),size(X16),'3412',1.000,
     & X16,S182)
       deallocate(S182)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S183(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S183)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X110(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X110=0.0d0
       call sum_stripe(4,shape(X110),size(X110),'3412',
     & 1.000,X110,S183)
       deallocate(S183)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S184(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S184)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X111(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       X111=0.0d0
       call sum_stripe(4,shape(X111),size(X111),'3412',
     & 1.000,X111,S184)
       deallocate(S184)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S185(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S185)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X112(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X112=0.0d0
       call sum_stripe(4,shape(X112),size(X112),'3412',
     & 1.000,X112,S185)
       deallocate(S185)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S186(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S186)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X113(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       X113=0.0d0
       call sum_stripe(4,shape(X113),size(X113),'3412',
     & 1.000,X113,S186)
       deallocate(S186)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(Q34(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q34)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X29),size(X29),'21',0.500,
     & X29,Q34)
       deallocate(Q34)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(Q35(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q35)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X30),size(X30),'21',0.500,
     & X30,Q35)
       deallocate(Q35)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N1/),'3421',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(Q36(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q36)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(M2+1:N3,N1+1:M2))
       call reorder_shift(2,shape(Q36),size(Q36),shape(B1),size(B1),
     & (/M2-M2,N1-N1/),'21',Q36,B1)
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M2-N1,M2-N2,N2-N2,N0-N0,M1-M1,N0-N0/),'312456',t3C1,F2)
       allocate(Z314(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K8*K5*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,B1,F2,Z314)
       deallocate(B1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124563',
     & 0.500,V3C,Z314)
       deallocate(Z314)
       deallocate(Q36)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(Q37(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q37)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N1+1:M2,N1+1:M2))
       call reorder_shift(2,shape(Q37),size(Q37),shape(B1),size(B1),
     & (/N1-N1,N1-N1/),'21',Q37,B1)
       allocate(F2(N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N1-N1,M2-N2,N2-N2,N0-N0,M1-M1,N0-N0/),'312456',t3C1,F2)
       allocate(Z315(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K8*K5*K0*K6
       I3=K9
       call EGEMM(I1,I2,I3,B1,F2,Z315)
       deallocate(B1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124563',
     & 0.500,V3C,Z315)
       deallocate(Z315)
       deallocate(Q37)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S187(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S187)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X114(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X114=0.0d0
       call sum_stripe(4,shape(X114),size(X114),'3412',
     & 1.000,X114,S187)
       deallocate(S187)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S188(N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S188)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X115(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       X115=0.0d0
       call sum_stripe(4,shape(X115),size(X115),'3412',
     & 1.000,X115,S188)
       deallocate(S188)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S189(N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S189)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X116(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       X116=0.0d0
       call sum_stripe(4,shape(X116),size(X116),'3412',
     & 1.000,X116,S189)
       deallocate(S189)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S190(N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S190)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X117(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       X117=0.0d0
       call sum_stripe(4,shape(X117),size(X117),'3412',
     & 1.000,X117,S190)
       deallocate(S190)
C
       allocate(B1(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,N1-N1/),'21',FAHP,B1)
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S191(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K1
       call EGEMM(I1,I2,I3,B1,D2,S191)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S191)
       deallocate(S191)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,M1-N0,N0-N0/),'2134',t2B,D2)
       allocate(S192(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S192)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S192)
       deallocate(S192)
C
       allocate(B1(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,N1-N1/),'21',FAHP,B1)
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S193(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K1
       call EGEMM(I1,I2,I3,B1,D2,S193)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S193)
       deallocate(S193)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,N0-N0,N0-N0/),'2134',t2B,D2)
       allocate(S194(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S194)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S194)
       deallocate(S194)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'3142',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S195(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S195)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2314',1.000,X4,
     & S195)
       deallocate(S195)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'3142',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S196(M2+1:N3,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S196)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S196)
       deallocate(S196)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'3142',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S197(M2+1:N3,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S197)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',1.000,X3,
     & S197)
       deallocate(S197)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'3142',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S198(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S198)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2314',1.000,X2,
     & S198)
       deallocate(S198)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S199(N2+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S199)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2413',-1.000,
     & X8,S199)
       deallocate(S199)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S200(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S200)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2413',-1.000,
     & X7,S200)
       deallocate(S200)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S201(N2+1:M2,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S201)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2413',-1.000,
     & X6,S201)
       deallocate(S201)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S202(M2+1:N3,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S202)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2413',-1.000,
     & X5,S202)
       deallocate(S202)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S203(N2+1:M2,N1+1:M2,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S203)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2314',1.000,X6,
     & S203)
       deallocate(S203)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S204(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S204)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2314',1.000,X5,
     & S204)
       deallocate(S204)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S205(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S205)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2314',1.000,
     & X10,S205)
       deallocate(S205)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'3241',VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S206(M2+1:N3,N0+1:M1,N0+1:N1,M1+1:N2))
       I1=K8*K1
       I2=K5*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S206)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2413',-1.000,
     & X1,S206)
       deallocate(S206)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S207(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S207)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2314',1.000,X9,
     & S207)
       deallocate(S207)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'3241',VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S208(N2+1:M2,N0+1:M1,N0+1:N1,M1+1:N2))
       I1=K8*K1
       I2=K5*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S208)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2413',-1.000,
     & X2,S208)
       deallocate(S208)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S209(N2+1:M2,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S209)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2314',1.000,X8,
     & S209)
       deallocate(S209)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S210(M2+1:N3,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S210)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2314',1.000,X7,
     & S210)
       deallocate(S210)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N2/),'4123',VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S211(N1+1:M2,N0+1:M1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S211)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'3412',-1.000,
     & X7,S211)
       deallocate(S211)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,M2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S212(N2+1:M2,N0+1:M1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S212)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'3412',1.000,
     & X12,S212)
       deallocate(S212)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,M2-N2/),'1243',VBHPPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',t2B,D2)
       allocate(S213(M1+1:N2,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K5*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S213)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'3412',1.000,X1,
     & S213)
       deallocate(S213)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N2-N2/),'4123',VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S214(N1+1:M2,N0+1:M1,N1+1:N3,N2+1:M2))
       I1=K0*K3
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S214)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'3412',-1.000,
     & X8,S214)
       deallocate(S214)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S215(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S215)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'2413',-1.000,
     & X12,S215)
       deallocate(S215)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,N2-N2/),'1243',VBHPPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',t2B,D2)
       allocate(S216(M1+1:N2,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1
       I2=K5*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S216)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'3412',1.000,X2,
     & S216)
       deallocate(S216)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S217(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S217)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2413',-1.000,
     & X10,S217)
       deallocate(S217)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'3241',VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S218(M2+1:N3,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S218)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2413',-1.000,
     & X3,S218)
       deallocate(S218)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S219(M2+1:N3,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S219)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2413',-1.000,
     & X9,S219)
       deallocate(S219)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'3241',VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S220(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S220)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2413',-1.000,
     & X4,S220)
       deallocate(S220)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N2/),'4123',VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S221(N1+1:M2,M1+1:N2,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S221)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'3412',-1.000,
     & X5,S221)
       deallocate(S221)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,M2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S222(N2+1:M2,M1+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S222)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'3412',1.000,
     & X11,S222)
       deallocate(S222)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,M2-N2/),'1243',VBHPPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',t2B,D2)
       allocate(S223(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K5*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S223)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'3412',1.000,X3,
     & S223)
       deallocate(S223)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N2-N2/),'4123',VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S224(N1+1:M2,M1+1:N2,N1+1:N3,N2+1:M2))
       I1=K0*K3
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S224)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'3412',-1.000,
     & X6,S224)
       deallocate(S224)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S225(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S225)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'2413',-1.000,
     & X11,S225)
       deallocate(S225)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,N2-N2/),'1243',VBHPPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',t2B,D2)
       allocate(S226(N0+1:M1,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1
       I2=K5*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S226)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'3412',1.000,X4,
     & S226)
       deallocate(S226)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',t2B,D2)
       allocate(S227(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S227)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',1.000,
     & X13,S227)
       deallocate(S227)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(D2(N2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',t2B,D2)
       allocate(S228(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S228)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'2341',1.000,
     & X14,S228)
       deallocate(S228)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S229(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S229)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S229)
       deallocate(S229)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,N2-N2/),'21',FBHP,B1)
       allocate(D2(N0+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S230(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,D2,S230)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X16),size(X16),'2341',-1.000,
     & X16,S230)
       deallocate(S230)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S231(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S231)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2314',1.000,X4,
     & S231)
       deallocate(S231)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S232(M2+1:N3,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S232)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',1.000,X3,
     & S232)
       deallocate(S232)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'4132',VBHHHP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S233(N1+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S233)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'2314',-1.000,
     & X13,S233)
       deallocate(S233)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S234(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S234)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2314',1.000,X2,
     & S234)
       deallocate(S234)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S235(M2+1:N3,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S235)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S235)
       deallocate(S235)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'4132',VBHHHP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S236(N1+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S236)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'2314',-1.000,
     & X14,S236)
       deallocate(S236)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S237(M2+1:N3,N1+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S237)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X15),size(X15),'2314',1.000,
     & X15,S237)
       deallocate(S237)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S238(N2+1:M2,N1+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S238)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X16),size(X16),'2314',1.000,
     & X16,S238)
       deallocate(S238)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S239(N2+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S239)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2413',1.000,X8,
     & S239)
       deallocate(S239)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S240(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S240)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2413',1.000,X7,
     & S240)
       deallocate(S240)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,N1-N1/),'1234',VBPHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',t2B,D2)
       allocate(S241(M1+1:N2,N0+1:M1,N0+1:N2,N1+1:M2))
       I1=K9*K2
       I2=K5*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S241)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'3412',1.000,
     & X13,S241)
       deallocate(S241)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S242(N2+1:M2,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S242)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2413',1.000,X6,
     & S242)
       deallocate(S242)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S243(M2+1:N3,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S243)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2413',1.000,X5,
     & S243)
       deallocate(S243)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,N1-N1/),'1234',VBPHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',t2B,D2)
       allocate(S244(N0+1:M1,N0+1:M1,N0+1:N2,N1+1:M2))
       I1=K9*K2
       I2=K5*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S244)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'3412',1.000,
     & X14,S244)
       deallocate(S244)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N1-N1/),'3214',VBPHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S245(M2+1:N3,N0+1:M1,N2+1:N3,N1+1:M2))
       I1=K9*K4
       I2=K5*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S245)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X15),size(X15),'2413',-1.000,
     & X15,S245)
       deallocate(S245)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N1-N1/),'3214',VBPHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S246(N2+1:M2,N0+1:M1,N2+1:N3,N1+1:M2))
       I1=K9*K4
       I2=K5*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S246)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X16),size(X16),'2413',-1.000,
     & X16,S246)
       deallocate(S246)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S247(M2+1:N3,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S247)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'2314',0.500,
     & X11,S247)
       deallocate(S247)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S248(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S248)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2314',1.000,
     & X10,S248)
       deallocate(S248)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S249(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S249)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2314',1.000,X9,
     & S249)
       deallocate(S249)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'4132',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S250(N1+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S250)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'2413',-1.000,
     & X13,S250)
       deallocate(S250)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S251(M2+1:N3,N2+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S251)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'2314',0.500,
     & X12,S251)
       deallocate(S251)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S252(N2+1:M2,N0+1:M1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S252)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'3412',-1.000,
     & X12,S252)
       deallocate(S252)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S253(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S253)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'2413',1.000,
     & X12,S253)
       deallocate(S253)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S254(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S254)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2413',-1.000,
     & X10,S254)
       deallocate(S254)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S255(M2+1:N3,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S255)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2413',-1.000,
     & X9,S255)
       deallocate(S255)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'4132',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S256(N1+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S256)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'2413',-1.000,
     & X14,S256)
       deallocate(S256)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S257(N2+1:M2,M1+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S257)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'3412',-1.000,
     & X11,S257)
       deallocate(S257)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S258(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S258)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'2413',1.000,
     & X11,S258)
       deallocate(S258)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,M2-N2/),'1243',VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S259(N0+1:M1,M1+1:N2,N0+1:N2,M2+1:N3))
       I1=K6*K2
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S259)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'3412',0.500,X9,
     & S259)
       deallocate(S259)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M2-N2/),'4213',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S260(N1+1:M2,N0+1:M1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S260)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X15),size(X15),'3412',1.000,
     & X15,S260)
       deallocate(S260)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N2-N2/),'1243',VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S261(N0+1:M1,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S261)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'3412',0.500,
     & X10,S261)
       deallocate(S261)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4213',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S262(N1+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S262)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X16),size(X16),'3412',1.000,
     & X16,S262)
       deallocate(S262)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,N2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S263(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S263)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S263)
       deallocate(S263)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N1,N2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S264(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S264)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',0.500,X8,
     & S264)
       deallocate(S264)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,N2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S265(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S265)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S265)
       deallocate(S265)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N1-N1,N2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S266(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S266)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',0.500,X8,
     & S266)
       deallocate(S266)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,M2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S267(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S267)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S267)
       deallocate(S267)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N1,M2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S268(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S268)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',0.500,X7,
     & S268)
       deallocate(S268)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,M2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S269(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S269)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S269)
       deallocate(S269)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N1-N1,M2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S270(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S270)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',0.500,X7,
     & S270)
       deallocate(S270)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,N2-N2,N1-N1,M1-M1/),'562134',t3B4,F2)
       allocate(S271(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S271)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',0.500,X6,
     & S271)
       deallocate(S271)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,N2-N2,N1-N1,M1-N0/),'562134',t3B1,F2)
       allocate(S272(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S272)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',1.000,X6,
     & S272)
       deallocate(S272)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N1,N2-N2,N1-N1,M1-N0/),'562134',t3B1,F2)
       allocate(S273(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S273)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',0.500,X6,
     & S273)
       deallocate(S273)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2,N1-N1,M1-M1/),'562134',t3B4,F2)
       allocate(S274(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S274)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',0.500,X6,
     & S274)
       deallocate(S274)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,N2-N2,N1-N1,M1-N0/),'562134',t3B1,F2)
       allocate(S275(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S275)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',1.000,X6,
     & S275)
       deallocate(S275)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N1-N1,N2-N2,N1-N1,M1-N0/),'562134',t3B1,F2)
       allocate(S276(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S276)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',0.500,X6,
     & S276)
       deallocate(S276)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,M2-N2,N1-N1,M1-M1/),'562134',t3B4,F2)
       allocate(S277(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S277)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',0.500,X5,
     & S277)
       deallocate(S277)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,M2-N2,N1-N1,M1-N0/),'562134',t3B1,F2)
       allocate(S278(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S278)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',1.000,X5,
     & S278)
       deallocate(S278)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N1,M2-N2,N1-N1,M1-N0/),'562134',t3B1,F2)
       allocate(S279(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S279)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',0.500,X5,
     & S279)
       deallocate(S279)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,M2-N2,N1-N1,M1-M1/),'562134',t3B4,F2)
       allocate(S280(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S280)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',0.500,X5,
     & S280)
       deallocate(S280)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,M2-N2,N1-N1,M1-N0/),'562134',t3B1,F2)
       allocate(S281(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S281)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',1.000,X5,
     & S281)
       deallocate(S281)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N1-N1,M2-N2,N1-N1,M1-N0/),'562134',t3B1,F2)
       allocate(S282(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S282)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',0.500,X5,
     & S282)
       deallocate(S282)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,M2-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/M1-M1,M2-M2,M2-M2,N2-N2,N0-N0,N0-N0/),'623145',t3B3,F2)
       allocate(S283(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S283)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-0.500,
     & X4,S283)
       deallocate(S283)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N1-N1,N2-N2,N0-N0,N0-N0/),'623145',t3B1,F2)
       allocate(S284(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S284)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S284)
       deallocate(S284)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N1-N1,N2-N2,N0-N0,N0-N0/),'623145',t3B1,F2)
       allocate(S285(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S285)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-0.500,
     & X4,S285)
       deallocate(S285)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S286(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S286)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X17),size(X17),'3412',1.000,
     & X17,S286)
       deallocate(S286)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S287(M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S287)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X18),size(X18),'3412',1.000,
     & X18,S287)
       deallocate(S287)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N1-N1,M2-N2,N0-N0,N0-N0/),'623145',t3B1,F2)
       allocate(S288(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S288)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S288)
       deallocate(S288)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N1-N1,M2-N2,N0-N0,N0-N0/),'623145',t3B1,F2)
       allocate(S289(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S289)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-0.500,
     & X3,S289)
       deallocate(S289)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S290(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S290)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X19),size(X19),'3412',1.000,
     & X19,S290)
       deallocate(S290)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S291(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S291)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X20),size(X20),'3412',1.000,
     & X20,S291)
       deallocate(S291)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,M2-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B2),size(t3B2),shape(F2),size(F2),
     & (/N0-N0,M2-M2,M2-M2,N2-N2,M1-M1,N0-N0/),'523146',t3B2,F2)
       allocate(S292(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S292)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',0.500,X2,
     & S292)
       deallocate(S292)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,M2-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/M1-M1,M2-M2,M2-M2,N2-N2,M1-N0,N0-N0/),'623145',t3B3,F2)
       allocate(S293(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S293)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-0.500,
     & X2,S293)
       deallocate(S293)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N1-N1,N2-N2,M1-M1,N0-N0/),'523146',t3B4,F2)
       allocate(S294(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S294)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S294)
       deallocate(S294)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N1-N1,N2-N2,M1-N0,N0-N0/),'623145',t3B1,F2)
       allocate(S295(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S295)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S295)
       deallocate(S295)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N1-N1,N2-N2,M1-M1,N0-N0/),'523146',t3B4,F2)
       allocate(S296(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S296)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',0.500,X2,
     & S296)
       deallocate(S296)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N1-N1,N2-N2,M1-N0,N0-N0/),'623145',t3B1,F2)
       allocate(S297(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S297)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-0.500,
     & X2,S297)
       deallocate(S297)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S298(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S298)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X21),size(X21),'3412',1.000,
     & X21,S298)
       deallocate(S298)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S299(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S299)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X22),size(X22),'3412',1.000,
     & X22,S299)
       deallocate(S299)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S300(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S300)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X23),size(X23),'3412',1.000,
     & X23,S300)
       deallocate(S300)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S301(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S301)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X24),size(X24),'3412',1.000,
     & X24,S301)
       deallocate(S301)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N1-N1,M2-N2,M1-M1,N0-N0/),'523146',t3B4,F2)
       allocate(S302(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S302)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S302)
       deallocate(S302)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N1-N1,M2-N2,M1-N0,N0-N0/),'623145',t3B1,F2)
       allocate(S303(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S303)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S303)
       deallocate(S303)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N1-N1,M2-N2,M1-M1,N0-N0/),'523146',t3B4,F2)
       allocate(S304(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S304)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',0.500,X1,
     & S304)
       deallocate(S304)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N1-N1,M2-N2,M1-N0,N0-N0/),'623145',t3B1,F2)
       allocate(S305(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S305)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-0.500,
     & X1,S305)
       deallocate(S305)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S306(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S306)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X25),size(X25),'3412',1.000,
     & X25,S306)
       deallocate(S306)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S307(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S307)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X26),size(X26),'3412',1.000,
     & X26,S307)
       deallocate(S307)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S308(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S308)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X27),size(X27),'3412',1.000,
     & X27,S308)
       deallocate(S308)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S309(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S309)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X28),size(X28),'3412',1.000,
     & X28,S309)
       deallocate(S309)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(S310(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S310)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S310)
       deallocate(S310)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,N2-N2,N1-N1,N0-N0/),'461235',t3C4,F2)
       allocate(S311(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S311)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S311)
       deallocate(S311)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,M2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(S312(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S312)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S312)
       deallocate(S312)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(S313(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S313)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S313)
       deallocate(S313)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,N2-N2,N1-N1,N0-N0/),'461235',t3C4,F2)
       allocate(S314(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S314)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S314)
       deallocate(S314)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,N2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(S315(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S315)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S315)
       deallocate(S315)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-M2,M2-M2,N1-N1,N0-N0/),'561234',t3C3,F2)
       allocate(S316(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S316)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S316)
       deallocate(S316)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C2),size(t3C2),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-M2,M2-M2,N1-N1,N0-N0/),'461235',t3C2,F2)
       allocate(S317(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S317)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',-1.000,
     & X7,S317)
       deallocate(S317)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-M1,M1-N0,M2-M2,M2-M2,N1-N1,N0-N0/),'561234',t3C3,F2)
       allocate(S318(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S318)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S318)
       deallocate(S318)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N2-N2,M2-N2,N1-N1,N0-N0/),'562134',t3C1,F2)
       allocate(S319(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S319)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',-1.000,
     & X7,S319)
       deallocate(S319)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,M2-N2,N1-N1,N0-N0/),'462135',t3C4,F2)
       allocate(S320(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S320)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S320)
       deallocate(S320)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,N2-N2,M2-N2,N1-N1,N0-N0/),'562134',t3C1,F2)
       allocate(S321(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S321)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',-1.000,
     & X7,S321)
       deallocate(S321)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-N1,M2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S322(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S322)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',1.000,
     & X12,S322)
       deallocate(S322)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,M2-N2,N2-N2,N0-N0/),'463125',t3C4,F2)
       allocate(S323(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S323)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',-1.000,
     & X12,S323)
       deallocate(S323)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,M2-N1,M2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S324(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S324)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',1.000,
     & X12,S324)
       deallocate(S324)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N1-N1,M2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S325(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S325)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',1.000,
     & X12,S325)
       deallocate(S325)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,M2-N2,N2-N2,N0-N0/),'463125',t3C4,F2)
       allocate(S326(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S326)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',-1.000,
     & X12,S326)
       deallocate(S326)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,N1-N1,M2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S327(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S327)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',1.000,
     & X12,S327)
       deallocate(S327)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',t2B,D2)
       allocate(S328(M1+1:N2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K5*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S328)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X44),size(X44),'3412',1.000,
     & X44,S328)
       deallocate(S328)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,M1-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',t2B,D2)
       allocate(S329(M1+1:N2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K5*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S329)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X45),size(X45),'3412',1.000,
     & X45,S329)
       deallocate(S329)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',t2B,D2)
       allocate(S330(M1+1:N2,N0+1:M1,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K5*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S330)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X46),size(X46),'3412',1.000,
     & X46,S330)
       deallocate(S330)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2,N1-N1,M1-M1/),'461235',t3C1,F2)
       allocate(S331(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S331)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S331)
       deallocate(S331)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N2,N2-N2,N1-N1,M1-M1/),'461235',t3C1,F2)
       allocate(S332(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S332)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S332)
       deallocate(S332)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,N2-N2,N1-N1,M1-M1/),'461235',t3C1,F2)
       allocate(S333(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S333)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S333)
       deallocate(S333)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,N2-N2,N1-N1,M1-M1/),'461235',t3C1,F2)
       allocate(S334(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S334)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S334)
       deallocate(S334)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2,N1-N1,M1-M1/),'461235',t3C1,F2)
       allocate(S335(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S335)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S335)
       deallocate(S335)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N2-N2,N2-N2,N1-N1,M1-M1/),'461235',t3C1,F2)
       allocate(S336(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S336)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S336)
       deallocate(S336)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,N2-N2,N1-N1,M1-M1/),'461235',t3C1,F2)
       allocate(S337(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S337)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S337)
       deallocate(S337)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,N2-N2,N1-N1,M1-M1/),'461235',t3C1,F2)
       allocate(S338(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S338)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S338)
       deallocate(S338)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-M2,M2-M2,N1-N1,M1-M1/),'461235',t3C3,F2)
       allocate(S339(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S339)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S339)
       deallocate(S339)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-M2,M2-M2,N1-N1,M1-M1/),'461235',t3C3,F2)
       allocate(S340(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S340)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S340)
       deallocate(S340)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-M2,M2-M2,N1-N1,M1-M1/),'461235',t3C3,F2)
       allocate(S341(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S341)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S341)
       deallocate(S341)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-M2,M2-M2,N1-N1,M1-M1/),'461235',t3C3,F2)
       allocate(S342(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S342)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S342)
       deallocate(S342)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,M2-N2,N1-N1,M1-M1/),'462135',t3C1,F2)
       allocate(S343(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S343)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',1.000,X5,
     & S343)
       deallocate(S343)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N2-N2,M2-N2,N1-N1,M1-M1/),'462135',t3C1,F2)
       allocate(S344(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S344)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',1.000,X5,
     & S344)
       deallocate(S344)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,M2-N2,N1-N1,M1-M1/),'462135',t3C1,F2)
       allocate(S345(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S345)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',1.000,X5,
     & S345)
       deallocate(S345)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,M2-N2,N1-N1,M1-M1/),'462135',t3C1,F2)
       allocate(S346(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S346)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',1.000,X5,
     & S346)
       deallocate(S346)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S347(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S347)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',-1.000,
     & X11,S347)
       deallocate(S347)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S348(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S348)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',-1.000,
     & X11,S348)
       deallocate(S348)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S349(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S349)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',-1.000,
     & X11,S349)
       deallocate(S349)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S350(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S350)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',-1.000,
     & X11,S350)
       deallocate(S350)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S351(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S351)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',-1.000,
     & X11,S351)
       deallocate(S351)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N1-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S352(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S352)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',-1.000,
     & X11,S352)
       deallocate(S352)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S353(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S353)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',-1.000,
     & X11,S353)
       deallocate(S353)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S354(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S354)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',-1.000,
     & X11,S354)
       deallocate(S354)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',t2B,D2)
       allocate(S355(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K5*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S355)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X47),size(X47),'3412',1.000,
     & X47,S355)
       deallocate(S355)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',t2B,D2)
       allocate(S356(N0+1:M1,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K5*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S356)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X48),size(X48),'3412',1.000,
     & X48,S356)
       deallocate(S356)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,M1-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',t2B,D2)
       allocate(S357(N0+1:M1,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K5*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S357)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X49),size(X49),'3412',1.000,
     & X49,S357)
       deallocate(S357)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',t2B,D2)
       allocate(S358(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K5*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S358)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X50),size(X50),'3412',1.000,
     & X50,S358)
       deallocate(S358)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S359(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S359)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S359)
       deallocate(S359)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S360(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S360)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S360)
       deallocate(S360)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S361(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S361)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S361)
       deallocate(S361)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S362(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S362)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S362)
       deallocate(S362)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S363(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S363)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S363)
       deallocate(S363)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S364(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S364)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S364)
       deallocate(S364)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S365(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S365)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S365)
       deallocate(S365)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S366(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S366)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S366)
       deallocate(S366)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S367(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S367)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X89),size(X89),'3412',-1.000,
     & X89,S367)
       deallocate(S367)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S368(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S368)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X90),size(X90),'3412',-1.000,
     & X90,S368)
       deallocate(S368)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S369(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S369)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X91),size(X91),'3412',-1.000,
     & X91,S369)
       deallocate(S369)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S370(M2+1:N3,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7
       I2=K5*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S370)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X92),size(X92),'3412',-1.000,
     & X92,S370)
       deallocate(S370)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,M2-N2,N0-N0,M1-M1/),'623145',t3C1,F2)
       allocate(S371(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S371)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',1.000,X9,
     & S371)
       deallocate(S371)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,M2-N2,N0-N0,M1-M1/),'623145',t3C1,F2)
       allocate(S372(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S372)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',1.000,X9,
     & S372)
       deallocate(S372)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,N1-N1,M2-M2,N0-N0,M1-M1/),'613245',t3C3,F2)
       allocate(S373(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S373)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S373)
       deallocate(S373)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,N1-N1,M2-M2,N0-N0,M1-M1/),'613245',t3C3,F2)
       allocate(S374(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S374)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S374)
       deallocate(S374)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,M2-N2,N0-N0,M1-M1/),'623145',t3C1,F2)
       allocate(S375(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S375)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',1.000,X9,
     & S375)
       deallocate(S375)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,M2-N2,N0-N0,M1-M1/),'623145',t3C1,F2)
       allocate(S376(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S376)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',1.000,X9,
     & S376)
       deallocate(S376)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S377(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S377)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X93),size(X93),'3412',-1.000,
     & X93,S377)
       deallocate(S377)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S378(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S378)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X94),size(X94),'3412',-1.000,
     & X94,S378)
       deallocate(S378)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S379(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S379)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X95),size(X95),'3412',-1.000,
     & X95,S379)
       deallocate(S379)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S380(N2+1:M2,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7
       I2=K5*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S380)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X96),size(X96),'3412',-1.000,
     & X96,S380)
       deallocate(S380)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S381(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S381)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X40(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X40=0.0d0
       call sum_stripe(4,shape(X40),size(X40),'3412',-1.000,
     & X40,S381)
       deallocate(S381)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,M1,X40,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z40(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X40,F2,Z40)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z40)
       deallocate(Z40)
       deallocate(X40)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S382(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S382)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X41(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       X41=0.0d0
       call sum_stripe(4,shape(X41),size(X41),'3412',-1.000,
     & X41,S382)
       deallocate(S382)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,M1,X41,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z41(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X41,F2,Z41)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z41)
       deallocate(Z41)
       deallocate(X41)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S383(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S383)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X42(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X42=0.0d0
       call sum_stripe(4,shape(X42),size(X42),'3412',-1.000,
     & X42,S383)
       deallocate(S383)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,M1,X42,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z42(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X42,F2,Z42)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z42)
       deallocate(Z42)
       deallocate(X42)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S384(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S384)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X43(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       X43=0.0d0
       call sum_stripe(4,shape(X43),size(X43),'3412',-1.000,
     & X43,S384)
       deallocate(S384)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,M1,X43,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z43(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X43,F2,Z43)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z43)
       deallocate(Z43)
       deallocate(X43)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(Q38(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q38)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X29),size(X29),'21',1.000,
     & X29,Q38)
       deallocate(Q38)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(Q39(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q39)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X30),size(X30),'21',1.000,
     & X30,Q39)
       deallocate(Q39)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N2-N2,N0-N0,N0-N0/),'513246',t3C1,F2)
       allocate(S385(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S385)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S385)
       deallocate(S385)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N2-N2,N0-N0,N0-N0/),'513246',t3C1,F2)
       allocate(S386(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S386)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S386)
       deallocate(S386)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N2-N2,N0-N0,N0-N0/),'513246',t3C1,F2)
       allocate(S387(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S387)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S387)
       deallocate(S387)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N2-N2,N0-N0,N0-N0/),'513246',t3C1,F2)
       allocate(S388(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S388)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S388)
       deallocate(S388)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S389(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S389)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S389),size(S389),shape(D1),size(D1),
     & (/M1-M1,M2-M2,M2-M2,M1-M1/),'3412',S389,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'512346',t3C1,F2)
       allocate(Z520(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K5*K9*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z520)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & 1.000,V3C,Z520)
       deallocate(Z520)
       deallocate(S389)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S390(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S390)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S390),size(S390),shape(D1),size(D1),
     & (/M1-M1,N2-N2,M2-M2,M1-M1/),'3412',S390,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'512346',t3C1,F2)
       allocate(Z521(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K5*K9*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z521)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & 1.000,V3C,Z521)
       deallocate(Z521)
       deallocate(S390)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,M2-N2,N0-N0,N0-N0/),'523146',t3C1,F2)
       allocate(S391(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S391)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',1.000,X3,
     & S391)
       deallocate(S391)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-M1,M2-M2,N1-N1,M2-M2,N0-N0,N0-N0/),'513246',t3C3,F2)
       allocate(S392(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S392)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S392)
       deallocate(S392)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,M2-N2,N0-N0,N0-N0/),'523146',t3C1,F2)
       allocate(S393(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S393)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',1.000,X3,
     & S393)
       deallocate(S393)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S394(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S394)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S394),size(S394),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,M1-M1/),'3412',S394,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-M1,M2-M2,M2-M2,N1-N1,N0-N0,N0-N0/),'512346',t3C3,F2)
       allocate(Z525(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z525)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & -1.000,V3C,Z525)
       deallocate(Z525)
       deallocate(S394)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S395(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S395)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S395),size(S395),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N2-N2,M1-M1/),'3412',S395,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N2,N1-N1,N0-N0,N0-N0/),'521346',t3C1,F2)
       allocate(Z526(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z526)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & 1.000,V3C,Z526)
       deallocate(Z526)
       deallocate(S395)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S396(N1+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S396)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X97),size(X97),'3412',1.000,
     & X97,S396)
       deallocate(S396)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S397(N1+1:M2,M1+1:N2,M1+1:N2,N1+1:M2))
       I1=K9*K8
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S397)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X98),size(X98),'3412',1.000,
     & X98,S397)
       deallocate(S397)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(Q40(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q40)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X99),size(X99),'21',1.000,
     & X99,Q40)
       deallocate(Q40)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N2-N2,M1-M1,N0-N0/),'413256',t3C1,F2)
       allocate(S398(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S398)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S398)
       deallocate(S398)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N2-N2,M1-M1,N0-N0/),'413256',t3C1,F2)
       allocate(S399(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S399)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S399)
       deallocate(S399)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N2-N2,M1-M1,N0-N0/),'413256',t3C1,F2)
       allocate(S400(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S400)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S400)
       deallocate(S400)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N2-N2,M1-M1,N0-N0/),'413256',t3C1,F2)
       allocate(S401(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S401)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S401)
       deallocate(S401)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N2-N2,M1-M1,N0-N0/),'413256',t3C1,F2)
       allocate(S402(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S402)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S402)
       deallocate(S402)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,N2-N2,M1-M1,N0-N0/),'413256',t3C1,F2)
       allocate(S403(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S403)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S403)
       deallocate(S403)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N2-N2,M1-M1,N0-N0/),'413256',t3C1,F2)
       allocate(S404(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S404)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S404)
       deallocate(S404)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,N2-N2,M1-M1,N0-N0/),'413256',t3C1,F2)
       allocate(S405(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S405)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S405)
       deallocate(S405)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S406(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S406)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S406),size(S406),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N0-N0/),'3412',S406,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(Z538(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K5*K8*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z538)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & 1.000,V3C,Z538)
       deallocate(Z538)
       deallocate(S406)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S407(M2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S407)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S407),size(S407),shape(D1),size(D1),
     & (/M1-M1,M2-M2,M2-M2,N0-N0/),'3412',S407,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(Z539(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K5*K8*K9*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z539)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & 1.000,V3C,Z539)
       deallocate(Z539)
       deallocate(S407)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S408(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S408)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S408),size(S408),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,N0-N0/),'3412',S408,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(Z540(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K5*K8*K9*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z540)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & 1.000,V3C,Z540)
       deallocate(Z540)
       deallocate(S408)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S409(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S409)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S409),size(S409),shape(D1),size(D1),
     & (/M1-M1,N2-N2,M2-M2,N0-N0/),'3412',S409,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(Z541(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K5*K8*K9*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z541)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & 1.000,V3C,Z541)
       deallocate(Z541)
       deallocate(S409)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,M2-N2,M1-M1,N0-N0/),'423156',t3C1,F2)
       allocate(S410(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S410)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S410)
       deallocate(S410)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,M2-N2,M1-M1,N0-N0/),'423156',t3C1,F2)
       allocate(S411(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S411)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S411)
       deallocate(S411)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,N1-N1,M2-M2,M1-M1,N0-N0/),'413256',t3C3,F2)
       allocate(S412(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S412)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S412)
       deallocate(S412)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,N1-N1,M2-M2,M1-M1,N0-N0/),'413256',t3C3,F2)
       allocate(S413(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S413)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S413)
       deallocate(S413)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,M2-N2,M1-M1,N0-N0/),'423156',t3C1,F2)
       allocate(S414(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S414)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S414)
       deallocate(S414)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,M2-N2,M1-M1,N0-N0/),'423156',t3C1,F2)
       allocate(S415(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S415)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S415)
       deallocate(S415)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S416(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S416)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S416),size(S416),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,N0-N0/),'3412',S416,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,M2-M2,N1-N1,M1-M1,N0-N0/),'412356',t3C3,F2)
       allocate(Z548(M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z548)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & -1.000,V3C,Z548)
       deallocate(Z548)
       deallocate(S416)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S417(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S417)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S417),size(S417),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,N0-N0/),'3412',S417,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,M2-M2,N1-N1,M1-M1,N0-N0/),'412356',t3C3,F2)
       allocate(Z549(M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z549)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & -1.000,V3C,Z549)
       deallocate(Z549)
       deallocate(S417)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S418(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S418)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S418),size(S418),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S418,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N1-N1,M1-M1,N0-N0/),'421356',t3C1,F2)
       allocate(Z550(M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z550)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & 1.000,V3C,Z550)
       deallocate(Z550)
       deallocate(S418)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S419(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S419)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S419),size(S419),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N2-N2,N0-N0/),'3412',S419,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N1-N1,M1-M1,N0-N0/),'421356',t3C1,F2)
       allocate(Z551(M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z551)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & 1.000,V3C,Z551)
       deallocate(Z551)
       deallocate(S419)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S420(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S420)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X100),size(X100),'3412',
     & 1.000,X100,S420)
       deallocate(S420)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S421(N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S421)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X101),size(X101),'3412',
     & 1.000,X101,S421)
       deallocate(S421)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S422(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S422)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X102),size(X102),'3412',
     & 1.000,X102,S422)
       deallocate(S422)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S423(N1+1:M2,N0+1:M1,M1+1:N2,N1+1:M2))
       I1=K9*K8
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S423)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X103),size(X103),'3412',
     & 1.000,X103,S423)
       deallocate(S423)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(Q41(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q41)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X104),size(X104),'21',1.000,
     & X104,Q41)
       deallocate(Q41)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(Q42(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q42)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X105),size(X105),'21',1.000,
     & X105,Q42)
       deallocate(Q42)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S424(M2+1:N3,N1+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S424)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X65),size(X65),'3412',1.000,
     & X65,S424)
       deallocate(S424)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S425(M2+1:N3,N1+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S425)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X66),size(X66),'3412',1.000,
     & X66,S425)
       deallocate(S425)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S426(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S426)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X67),size(X67),'3412',1.000,
     & X67,S426)
       deallocate(S426)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S427(M2+1:N3,N1+1:M2,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S427)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X68),size(X68),'3412',1.000,
     & X68,S427)
       deallocate(S427)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',t2B,D2)
       allocate(Q43(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q43)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X106),size(X106),'21',-1.000,
     & X106,Q43)
       deallocate(Q43)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',t2B,D2)
       allocate(Q44(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q44)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X107),size(X107),'21',-1.000,
     & X107,Q44)
       deallocate(Q44)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S428(N2+1:M2,N1+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S428)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X69),size(X69),'3412',1.000,
     & X69,S428)
       deallocate(S428)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S429(N2+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S429)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X70),size(X70),'3412',1.000,
     & X70,S429)
       deallocate(S429)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S430(N2+1:M2,N1+1:M2,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S430)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X71),size(X71),'3412',1.000,
     & X71,S430)
       deallocate(S430)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',t2B,D2)
       allocate(Q45(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q45)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X108),size(X108),'21',-1.000,
     & X108,Q45)
       deallocate(Q45)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',t2B,D2)
       allocate(Q46(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q46)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X109),size(X109),'21',-1.000,
     & X109,Q46)
       deallocate(Q46)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(Q47(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q47)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X31),size(X31),'21',-1.000,
     & X31,Q47)
       deallocate(Q47)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(Q48(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q48)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X32),size(X32),'21',-1.000,
     & X32,Q48)
       deallocate(Q48)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,M2-N2,N2-N2,N0-N0/),'461235',t3D,F2)
       allocate(S431(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S431)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',1.000,
     & X12,S431)
       deallocate(S431)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,M2-N2,N2-N2,N0-N0/),'561234',t3D,F2)
       allocate(S432(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S432)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',-0.500,
     & X12,S432)
       deallocate(S432)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,M2-N2,N2-N2,N0-N0/),'462135',t3D,F2)
       allocate(S433(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S433)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',-1.000,
     & X12,S433)
       deallocate(S433)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,M2-N2,N2-N2,N0-N0/),'562134',t3D,F2)
       allocate(S434(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S434)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',0.500,
     & X12,S434)
       deallocate(S434)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,M2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S435(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S435)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',-0.500,
     & X11,S435)
       deallocate(S435)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,M2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S436(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S436)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',-1.000,
     & X11,S436)
       deallocate(S436)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,M2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S437(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S437)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',-0.500,
     & X11,S437)
       deallocate(S437)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,M2-N2,N2-N2,M1-M1/),'452136',t3D,F2)
       allocate(S438(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S438)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',0.500,
     & X11,S438)
       deallocate(S438)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,M2-N2,N2-N2,M1-M1/),'452136',t3D,F2)
       allocate(S439(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S439)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',1.000,
     & X11,S439)
       deallocate(S439)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,M2-N2,N2-N2,M1-M1/),'452136',t3D,F2)
       allocate(S440(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S440)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',0.500,
     & X11,S440)
       deallocate(S440)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(S441(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S441)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-0.500,
     & X10,S441)
       deallocate(S441)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(S442(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S442)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',0.500,
     & X10,S442)
       deallocate(S442)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(S443(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S443)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S443)
       deallocate(S443)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(S444(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S444)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',1.000,
     & X10,S444)
       deallocate(S444)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(S445(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S445)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-0.500,
     & X10,S445)
       deallocate(S445)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(S446(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S446)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',0.500,
     & X10,S446)
       deallocate(S446)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,M2-N2,N0-N0,M1-M1/),'413256',t3D,F2)
       allocate(S447(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S447)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',1.000,X9,
     & S447)
       deallocate(S447)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,M2-N2,N0-N0,M1-M1/),'513246',t3D,F2)
       allocate(S448(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S448)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S448)
       deallocate(S448)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,M2-N2,N0-N0,M1-M1/),'423156',t3D,F2)
       allocate(S449(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S449)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-0.500,
     & X9,S449)
       deallocate(S449)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,M2-N2,N0-N0,M1-M1/),'523146',t3D,F2)
       allocate(S450(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S450)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',0.500,X9,
     & S450)
       deallocate(S450)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S451(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S451)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X114),size(X114),'3412',
     & 1.000,X114,S451)
       deallocate(S451)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S452(N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S452)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X115),size(X115),'3412',
     & 1.000,X115,S452)
       deallocate(S452)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S453(N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S453)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X116),size(X116),'3412',
     & 1.000,X116,S453)
       deallocate(S453)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S454(N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S454)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X117),size(X117),'3412',
     & 1.000,X117,S454)
       deallocate(S454)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-N1,N2-N2,N1-N1,N0-N0/),'452136',t3B4,F2)
       allocate(S455(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S455)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X16),size(X16),'2341',-1.000,
     & X16,S455)
       deallocate(S455)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,N2-N2,N1-N1,N0-N0/),'462135',t3B1,F2)
       allocate(S456(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S456)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X16),size(X16),'2341',1.000,
     & X16,S456)
       deallocate(S456)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N1,N2-N2,N1-N1,N0-N0/),'462135',t3B1,F2)
       allocate(S457(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S457)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X16),size(X16),'2341',1.000,
     & X16,S457)
       deallocate(S457)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N1-N1,N2-N2,N1-N1,N0-N0/),'452136',t3B4,F2)
       allocate(S458(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S458)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X16),size(X16),'2341',-1.000,
     & X16,S458)
       deallocate(S458)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,N2-N2,N1-N1,N0-N0/),'462135',t3B1,F2)
       allocate(S459(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S459)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X16),size(X16),'2341',1.000,
     & X16,S459)
       deallocate(S459)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N1-N1,N2-N2,N1-N1,N0-N0/),'462135',t3B1,F2)
       allocate(S460(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S460)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X16),size(X16),'2341',1.000,
     & X16,S460)
       deallocate(S460)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-N1,M2-N2,N1-N1,N0-N0/),'452136',t3B4,F2)
       allocate(S461(M2+1:N3,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S461)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X15),size(X15),'2341',-1.000,
     & X15,S461)
       deallocate(S461)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,M2-N2,N1-N1,N0-N0/),'462135',t3B1,F2)
       allocate(S462(M2+1:N3,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S462)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X15),size(X15),'2341',1.000,
     & X15,S462)
       deallocate(S462)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N1,M2-N2,N1-N1,N0-N0/),'462135',t3B1,F2)
       allocate(S463(M2+1:N3,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S463)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X15),size(X15),'2341',1.000,
     & X15,S463)
       deallocate(S463)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N1-N1,M2-N2,N1-N1,N0-N0/),'452136',t3B4,F2)
       allocate(S464(M2+1:N3,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S464)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X15),size(X15),'2341',-1.000,
     & X15,S464)
       deallocate(S464)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,M2-N2,N1-N1,N0-N0/),'462135',t3B1,F2)
       allocate(S465(M2+1:N3,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S465)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X15),size(X15),'2341',1.000,
     & X15,S465)
       deallocate(S465)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N1-N1,M2-N2,N1-N1,N0-N0/),'462135',t3B1,F2)
       allocate(S466(M2+1:N3,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S466)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X15),size(X15),'2341',1.000,
     & X15,S466)
       deallocate(S466)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'612345',t3B1,F2)
       allocate(S467(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S467)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X14),size(X14),'2341',-1.000,
     & X14,S467)
       deallocate(S467)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'612345',t3B1,F2)
       allocate(S468(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S468)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X14),size(X14),'2341',-1.000,
     & X14,S468)
       deallocate(S468)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'612345',t3B1,F2)
       allocate(S469(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S469)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X14),size(X14),'2341',-1.000,
     & X14,S469)
       deallocate(S469)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'612345',t3B1,F2)
       allocate(S470(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S470)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X14),size(X14),'2341',-1.000,
     & X14,S470)
       deallocate(S470)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S471(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S471)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X17),size(X17),'3412',1.000,
     & X17,S471)
       deallocate(S471)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S472(M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S472)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X18),size(X18),'3412',1.000,
     & X18,S472)
       deallocate(S472)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S473(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S473)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X19),size(X19),'3412',1.000,
     & X19,S473)
       deallocate(S473)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S474(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S474)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X20),size(X20),'3412',1.000,
     & X20,S474)
       deallocate(S474)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,M1-M1,N0-N0/),'512346',t3B4,F2)
       allocate(S475(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S475)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',1.000,
     & X13,S475)
       deallocate(S475)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,M1-N0,N0-N0/),'612345',t3B1,F2)
       allocate(S476(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S476)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',-1.000,
     & X13,S476)
       deallocate(S476)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,M1-M1,N0-N0/),'512346',t3B4,F2)
       allocate(S477(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S477)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',1.000,
     & X13,S477)
       deallocate(S477)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,M1-N0,N0-N0/),'612345',t3B1,F2)
       allocate(S478(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S478)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',-1.000,
     & X13,S478)
       deallocate(S478)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N1-N1,M1-M1,N0-N0/),'512346',t3B4,F2)
       allocate(S479(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S479)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',1.000,
     & X13,S479)
       deallocate(S479)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N1-N1,M1-N0,N0-N0/),'612345',t3B1,F2)
       allocate(S480(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S480)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',-1.000,
     & X13,S480)
       deallocate(S480)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N1-N1,M1-M1,N0-N0/),'512346',t3B4,F2)
       allocate(S481(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S481)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',1.000,
     & X13,S481)
       deallocate(S481)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N1-N1,M1-N0,N0-N0/),'612345',t3B1,F2)
       allocate(S482(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S482)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',-1.000,
     & X13,S482)
       deallocate(S482)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S483(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S483)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X21),size(X21),'3412',1.000,
     & X21,S483)
       deallocate(S483)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S484(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S484)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X22),size(X22),'3412',1.000,
     & X22,S484)
       deallocate(S484)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S485(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S485)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X23),size(X23),'3412',1.000,
     & X23,S485)
       deallocate(S485)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S486(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S486)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X24),size(X24),'3412',1.000,
     & X24,S486)
       deallocate(S486)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S487(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S487)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X25),size(X25),'3412',1.000,
     & X25,S487)
       deallocate(S487)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S488(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S488)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X26),size(X26),'3412',1.000,
     & X26,S488)
       deallocate(S488)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S489(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S489)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X27),size(X27),'3412',1.000,
     & X27,S489)
       deallocate(S489)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S490(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S490)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X28),size(X28),'3412',1.000,
     & X28,S490)
       deallocate(S490)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,N2-N2,N1-N1,N0-N0/),'451236',t3C1,F2)
       allocate(S491(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S491)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X16),size(X16),'2341',1.000,
     & X16,S491)
       deallocate(S491)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,N2-N2,N1-N1,N0-N0/),'451236',t3C1,F2)
       allocate(S492(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S492)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X16),size(X16),'2341',0.500,
     & X16,S492)
       deallocate(S492)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,N2-N2,N1-N1,N0-N0/),'451236',t3C1,F2)
       allocate(S493(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S493)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X16),size(X16),'2341',1.000,
     & X16,S493)
       deallocate(S493)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,N2-N2,N1-N1,N0-N0/),'451236',t3C1,F2)
       allocate(S494(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S494)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X16),size(X16),'2341',0.500,
     & X16,S494)
       deallocate(S494)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-M2,M2-M2,N1-N1,N0-N0/),'451236',t3C3,F2)
       allocate(S495(M2+1:N3,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S495)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X15),size(X15),'2341',1.000,
     & X15,S495)
       deallocate(S495)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-M2,M2-M2,N1-N1,N0-N0/),'451236',t3C3,F2)
       allocate(S496(M2+1:N3,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S496)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X15),size(X15),'2341',0.500,
     & X15,S496)
       deallocate(S496)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,M2-N2,N1-N1,N0-N0/),'452136',t3C1,F2)
       allocate(S497(M2+1:N3,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S497)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X15),size(X15),'2341',-1.000,
     & X15,S497)
       deallocate(S497)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,M2-N2,N1-N1,N0-N0/),'452136',t3C1,F2)
       allocate(S498(M2+1:N3,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S498)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X15),size(X15),'2341',-0.500,
     & X15,S498)
       deallocate(S498)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S499(N0+1:M1,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S499)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X72),size(X72),'3412',0.500,
     & X72,S499)
       deallocate(S499)
C
       allocate(D1(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S500(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S500)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X73),size(X73),'3412',0.500,
     & X73,S500)
       deallocate(S500)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-M1,M2-M2,M2-M2,N1-N1,N0-N0,N0-N0/),'512346',t3C3,F2)
       allocate(S501(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S501)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X14),size(X14),'2341',0.500,
     & X14,S501)
       deallocate(S501)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'512346',t3C1,F2)
       allocate(S502(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S502)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X14),size(X14),'2341',1.000,
     & X14,S502)
       deallocate(S502)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'512346',t3C1,F2)
       allocate(S503(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S503)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X14),size(X14),'2341',0.500,
     & X14,S503)
       deallocate(S503)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S504(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S504)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X74),size(X74),'3412',-1.000,
     & X74,S504)
       deallocate(S504)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S505(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S505)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X75),size(X75),'3412',-1.000,
     & X75,S505)
       deallocate(S505)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S506(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S506)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X76),size(X76),'3412',-1.000,
     & X76,S506)
       deallocate(S506)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S507(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S507)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X77),size(X77),'3412',-1.000,
     & X77,S507)
       deallocate(S507)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Q49(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q49)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X33),size(X33),'21',0.500,
     & X33,Q49)
       deallocate(Q49)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,M2-M2,N1-N1,M1-M1,N0-N0/),'412356',t3C3,F2)
       allocate(S508(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S508)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',-0.500,
     & X13,S508)
       deallocate(S508)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,M2-M2,N1-N1,M1-M1,N0-N0/),'412356',t3C3,F2)
       allocate(S509(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S509)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',-0.500,
     & X13,S509)
       deallocate(S509)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(S510(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S510)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',-1.000,
     & X13,S510)
       deallocate(S510)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(S511(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S511)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',-1.000,
     & X13,S511)
       deallocate(S511)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(S512(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S512)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',-0.500,
     & X13,S512)
       deallocate(S512)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(S513(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S513)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',-0.500,
     & X13,S513)
       deallocate(S513)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S514(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S514)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X78),size(X78),'3412',-1.000,
     & X78,S514)
       deallocate(S514)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S515(M2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S515)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X79),size(X79),'3412',-1.000,
     & X79,S515)
       deallocate(S515)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S516(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S516)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X80),size(X80),'3412',-1.000,
     & X80,S516)
       deallocate(S516)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S517(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S517)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X81),size(X81),'3412',-1.000,
     & X81,S517)
       deallocate(S517)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S518(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S518)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X82),size(X82),'3412',-1.000,
     & X82,S518)
       deallocate(S518)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S519(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S519)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X83),size(X83),'3412',-1.000,
     & X83,S519)
       deallocate(S519)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S520(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S520)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X84),size(X84),'3412',-1.000,
     & X84,S520)
       deallocate(S520)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S521(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S521)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X85),size(X85),'3412',-1.000,
     & X85,S521)
       deallocate(S521)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(Q50(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q50)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X34),size(X34),'21',0.500,
     & X34,Q50)
       deallocate(Q50)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(Q51(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q51)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X35),size(X35),'21',0.500,
     & X35,Q51)
       deallocate(Q51)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S522(M2+1:N3,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S522)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X86),size(X86),'3412',0.500,
     & X86,S522)
       deallocate(S522)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S523(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S523)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X87),size(X87),'3412',0.500,
     & X87,S523)
       deallocate(S523)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S524(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S524)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X88),size(X88),'3412',0.500,
     & X88,S524)
       deallocate(S524)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,M2-N2/),'3412',t2C,D2)
       allocate(Q52(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q52)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X36),size(X36),'21',-0.500,
     & X36,Q52)
       deallocate(Q52)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,M2-N2/),'3412',t2C,D2)
       allocate(Q53(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q53)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X37),size(X37),'21',-0.500,
     & X37,Q53)
       deallocate(Q53)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(Q54(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q54)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X38),size(X38),'21',-0.500,
     & X38,Q54)
       deallocate(Q54)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(Q55(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q55)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X39),size(X39),'21',-0.500,
     & X39,Q55)
       deallocate(Q55)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M1-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S525(N0+1:M1,N0+1:N2,N0+1:N1,M1+1:N2))
       I1=K8*K1*K2
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S525)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S525),size(S525),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,N0-N0/),'2341',S525,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S549(M2+1:N3,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S549)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S549)
       deallocate(S549)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S525),size(S525),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,N0-N0/),'2341',S525,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S550(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S550)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S550)
       deallocate(S550)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S525),size(S525),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,N0-N0/),'3241',S525,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S526(N1+1:M2,N0+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S526)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X13),size(X13),'2134',-1.000,
     & X13,S526)
       deallocate(S526)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,N2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S531(N0+1:M1,N0+1:N1,N2+1:N3,N2+1:M2))
       I1=K0*K4*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S531)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S531),size(S531),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,N0-N0/),'3241',S531,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S552(M1+1:N2,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S552)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'3124',1.000,X2,
     & S552)
       deallocate(S552)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S531),size(S531),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,N0-N0/),'3241',S531,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S556(N0+1:M1,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S556)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'3124',1.000,X4,
     & S556)
       deallocate(S556)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S531),size(S531),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2341',S531,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S532(N1+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S532)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X16),size(X16),'3124',-1.000,
     & X16,S532)
       deallocate(S532)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S537(N0+1:M1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S537)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S537),size(S537),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'3241',S537,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S538(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S538)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X112),size(X112),'3124',
     & -1.000,X112,S538)
       deallocate(S538)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z310(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X112,F2,Z310)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z310)
       deallocate(Z310)
       deallocate(X112)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S539(N0+1:M1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S539)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S539),size(S539),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,N0-N0/),'3241',S539,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S540(N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S540)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X113),size(X113),'3124',
     & -1.000,X113,S540)
       deallocate(S540)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z311(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X113,F2,Z311)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z311)
       deallocate(Z311)
       deallocate(X113)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q56(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q56)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q56),size(Q56),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q56,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(Q57(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q57)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X29),size(X29),'21',1.000,
     & X29,Q57)
       deallocate(Q57)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q58(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q58)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(Q58),size(Q58),shape(B1),size(B1),
     & (/N1-N1,M1-M1/),'21',Q58,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(Q59(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q59)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X30),size(X30),'21',1.000,
     & X30,Q59)
       deallocate(Q59)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q60(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q60)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q61(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q60,B2,Q61)
       deallocate(B2)
C
       call sum_stripe(2,shape(X31),size(X31),'21',-1.000,
     & X31,Q61)
       deallocate(Q61)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q62(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q62)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q63(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q62,B2,Q63)
       deallocate(B2)
C
       call sum_stripe(2,shape(X32),size(X32),'21',-1.000,
     & X32,Q63)
       deallocate(Q63)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S541(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S541)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S541),size(S541),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'3241',S541,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S542(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S542)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X114),size(X114),'3124',
     & -1.000,X114,S542)
       deallocate(S542)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z316(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X114,F2,Z316)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z316)
       deallocate(Z316)
       deallocate(X114)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S543(N0+1:M1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S543)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S543),size(S543),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'3241',S543,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S544(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S544)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X115),size(X115),'3124',
     & -1.000,X115,S544)
       deallocate(S544)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z317(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X115,F2,Z317)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z317)
       deallocate(Z317)
       deallocate(X115)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S545(N0+1:M1,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S545)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S545),size(S545),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'3241',S545,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S546(N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S546)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X116),size(X116),'3124',
     & -1.000,X116,S546)
       deallocate(S546)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'421356',t3D,F2)
       allocate(Z318(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X116,F2,Z318)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z318)
       deallocate(Z318)
       deallocate(X116)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S547(N0+1:M1,M1+1:N2,N0+1:N1,N2+1:M2))
       I1=K0*K1*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S547)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S547),size(S547),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,N0-N0/),'3241',S547,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S548(N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S548)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X117),size(X117),'3124',
     & -1.000,X117,S548)
       deallocate(S548)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'521346',t3D,F2)
       allocate(Z319(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X117,F2,Z319)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z319)
       deallocate(Z319)
       deallocate(X117)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N0-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S527(N0+1:M1,N0+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1*K2
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S527)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2341',S527,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S553(M2+1:N3,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S553)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',-1.000,
     & X3,S553)
       deallocate(S553)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2341',S527,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S554(N2+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S554)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',-1.000,
     & X4,S554)
       deallocate(S554)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3241',S527,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S528(N1+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S528)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X14),size(X14),'2134',-1.000,
     & X14,S528)
       deallocate(S528)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M2-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S533(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S533)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S533),size(S533),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'3241',S533,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S534(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S534)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X110),size(X110),'3124',
     & -1.000,X110,S534)
       deallocate(S534)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z308(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X110,F2,Z308)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z308)
       deallocate(Z308)
       deallocate(X110)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,M2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S529(N0+1:M1,N0+1:N1,N2+1:N3,M2+1:N3))
       I1=K6*K4*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S529)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S529),size(S529),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M2-M2,N0-N0/),'3241',S529,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S551(M1+1:N2,N0+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S551)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'3124',1.000,X1,
     & S551)
       deallocate(S551)
C
       allocate(D1(N2+1:N3,N0+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S529),size(S529),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M2-M2,N0-N0/),'3241',S529,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S555(N0+1:M1,N0+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K1
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S555)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'3124',1.000,X3,
     & S555)
       deallocate(S555)
C
       allocate(D1(N0+1:N1,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S529),size(S529),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,N0-N0/),'2341',S529,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S530(N1+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S530)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X15),size(X15),'3124',-1.000,
     & X15,S530)
       deallocate(S530)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,M2-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S535(N0+1:M1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S535)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S535),size(S535),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'3241',S535,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S536(N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S536)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X111),size(X111),'3124',
     & -1.000,X111,S536)
       deallocate(S536)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z309(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X111,F2,Z309)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z309)
       deallocate(Z309)
       deallocate(X111)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S557(N1+1:M2,N0+1:N2,N1+1:N3,M1+1:N2))
       I1=K8*K3*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S557)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S557),size(S557),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2314',S557,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S559(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S559)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'2134',1.000,X6,
     & S559)
       deallocate(S559)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S557),size(S557),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2314',S557,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S558(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S558)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',1.000,X5,
     & S558)
       deallocate(S558)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S560(M1+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S560)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S560),size(S560),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'2341',S560,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S561(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S561)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'3124',-1.000,
     & X5,S561)
       deallocate(S561)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S562(M1+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       I1=K0*K3*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S562)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S562),size(S562),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,M1-M1/),'2341',S562,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S563(N1+1:M2,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S563)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'3124',-1.000,
     & X6,S563)
       deallocate(S563)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S564(N1+1:M2,N0+1:N2,N1+1:N3,N0+1:M1))
       I1=K5*K3*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S564)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S564),size(S564),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2314',S564,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S566(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S566)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',1.000,X8,
     & S566)
       deallocate(S566)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S564),size(S564),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2314',S564,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S565(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S565)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',1.000,X7,
     & S565)
       deallocate(S565)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S567(N0+1:M1,N0+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3*K1
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S567)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S567),size(S567),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,N0-N0/),'2341',S567,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S568(N1+1:M2,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S568)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'3124',-1.000,
     & X7,S568)
       deallocate(S568)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S569(N0+1:M1,N0+1:N1,N1+1:N3,N2+1:M2))
       I1=K0*K3*K1
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S569)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S569),size(S569),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'2341',S569,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S570(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S570)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'3124',-1.000,
     & X8,S570)
       deallocate(S570)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N0-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S571(M1+1:N2,N0+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S571)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S571),size(S571),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,N0-N0/),'2314',S571,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S667(M2+1:N3,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S667)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S667)
       deallocate(S667)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S571),size(S571),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,N0-N0/),'2314',S571,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S668(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S668)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S668)
       deallocate(S668)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S571),size(S571),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,N0-N0/),'3214',S571,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S572(N1+1:M2,N0+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S572)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X13),size(X13),'2134',-1.000,
     & X13,S572)
       deallocate(S572)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S575(N1+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S575)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S575),size(S575),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'2314',S575,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S576(M2+1:N3,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S576)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X15),size(X15),'2134',1.000,
     & X15,S576)
       deallocate(S576)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S575),size(S575),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'2314',S575,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S577(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S577)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X16),size(X16),'2134',1.000,
     & X16,S577)
       deallocate(S577)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S578(N0+1:M1,N0+1:N2,N2+1:N3,N1+1:M2))
       I1=K9*K4*K2
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S578)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S578),size(S578),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'2341',S578,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S581(M2+1:N3,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S581)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X15),size(X15),'2134',-1.000,
     & X15,S581)
       deallocate(S581)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S578),size(S578),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'2341',S578,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S582(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S582)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X16),size(X16),'2134',-1.000,
     & X16,S582)
       deallocate(S582)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S578),size(S578),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N1-N1,N0-N0/),'3241',S578,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S579(M1+1:N2,N0+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S579)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X13),size(X13),'3124',1.000,
     & X13,S579)
       deallocate(S579)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S578),size(S578),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N1-N1,N0-N0/),'3241',S578,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S580(N0+1:M1,N0+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S580)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X14),size(X14),'3124',1.000,
     & X14,S580)
       deallocate(S580)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S585(N0+1:M1,N0+1:M1,M1+1:N1,N2+1:N3))
       I1=K4*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S585)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S585),size(S585),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M1-M1,N0-N0/),'4231',S585,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S592(N0+1:M1,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S592)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X49),size(X49),'3124',1.000,
     & X49,S592)
       deallocate(S592)
C
       call sumx_sorted3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,N0,M1,N0,M1,X49,VBHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,N2-N2,N1-N1,M1-M1/),'461235',t3C1,F2)
       allocate(Z49(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K9*K0*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,X49,F2,Z49)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123546',
     & 1.000,V3C,Z49)
       deallocate(Z49)
       deallocate(X49)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S585),size(S585),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M1-M1,N0-N0/),'4231',S585,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S586(M1+1:N2,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S586)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X45),size(X45),'3124',1.000,
     & X45,S586)
       deallocate(S586)
C
       call sumx_sorted3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,M1,N2,N0,M1,X45,VBHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,N2-N2,N1-N1,N0-N0/),'461235',t3C4,F2)
       allocate(Z45(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K5*K9*K0*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,X45,F2,Z45)
       deallocate(F2)
C
       V3C=V3C-Z45
       deallocate(Z45)
       deallocate(X45)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S589(N0+1:M1,N0+1:M1,N0+1:M1,N2+1:N3))
       I1=K4*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S589)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S589),size(S589),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N0-N0,N0-N0/),'4231',S589,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S590(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S590)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X47),size(X47),'3124',1.000,
     & X47,S590)
       deallocate(S590)
C
       call sumx_sorted3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,M1,N0,M1,N0,M1,N0,M1,X47,VBHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2,N1-N1,M1-M1/),'461235',t3C1,F2)
       allocate(Z47(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K9*K0*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,X47,F2,Z47)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123546',
     & 1.000,V3C,Z47)
       deallocate(Z47)
       deallocate(X47)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S583(N0+1:M1,M1+1:N2,N0+1:M1,N2+1:N3))
       I1=K4*K5*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S583)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S583),size(S583),shape(D1),size(D1),
     & (/N2-N2,M1-M1,N0-N0,N0-N0/),'4231',S583,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S584(M1+1:N2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S584)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X44),size(X44),'3124',1.000,
     & X44,S584)
       deallocate(S584)
C
       call sumx_sorted3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,M1,N2,N0,M1,X44,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(Z44(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K5*K9*K0*K6
       I3=K5*K8
       call EGEMM(I1,I2,I3,X44,F2,Z44)
       deallocate(F2)
C
       V3C=V3C+Z44
       deallocate(Z44)
       deallocate(X44)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S583),size(S583),shape(D1),size(D1),
     & (/N2-N2,M1-M1,N0-N0,N0-N0/),'4231',S583,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S591(N0+1:M1,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S591)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X48),size(X48),'3124',1.000,
     & X48,S591)
       deallocate(S591)
C
       call sumx_sorted3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,N0,M1,N0,M1,X48,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N2,N2-N2,N1-N1,M1-M1/),'461235',t3C1,F2)
       allocate(Z48(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K9*K0*K6
       I3=K5*K8
       call EGEMM(I1,I2,I3,X48,F2,Z48)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123546',
     & 1.000,V3C,Z48)
       deallocate(Z48)
       deallocate(X48)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S587(N0+1:M1,M1+1:N2,M1+1:N1,N2+1:N3))
       I1=K4*K7*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S587)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S587),size(S587),shape(D1),size(D1),
     & (/N2-N2,M1-M1,M1-M1,N0-N0/),'4231',S587,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S588(M1+1:N2,M1+1:N2,M1+1:N1,N0+1:M1))
       I1=K5*K7*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S588)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X46),size(X46),'3124',1.000,
     & X46,S588)
       deallocate(S588)
C
       call sumx_sorted3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,M1,N2,N0,M1,X46,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,M2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(Z46(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K5*K9*K0*K6
       I3=K7*K8
       call EGEMM(I1,I2,I3,X46,F2,Z46)
       deallocate(F2)
C
       V3C=V3C+Z46
       deallocate(Z46)
       deallocate(X46)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S587),size(S587),shape(D1),size(D1),
     & (/N2-N2,M1-M1,M1-M1,N0-N0/),'4231',S587,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S593(N0+1:M1,M1+1:N2,M1+1:N1,N0+1:M1))
       I1=K5*K7*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S593)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X50),size(X50),'3124',1.000,
     & X50,S593)
       deallocate(S593)
C
       call sumx_sorted3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,N0,M1,N0,M1,X50,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,N2-N2,N1-N1,M1-M1/),'461235',t3C1,F2)
       allocate(Z50(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K9*K0*K6
       I3=K7*K8
       call EGEMM(I1,I2,I3,X50,F2,Z50)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123546',
     & 1.000,V3C,Z50)
       deallocate(Z50)
       deallocate(X50)
C
       allocate(D1(N1+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S596(N0+1:M1,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S596)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S596),size(S596),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S596,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S603(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S603)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X94),size(X94),'3124',-1.000,
     & X94,S603)
       deallocate(S603)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,M2-M2,N1-N1,N0-N0,M1-M1/),'612345',t3C3,F2)
       allocate(Z137(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X94,F2,Z137)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134526',
     & 1.000,V3C,Z137)
       deallocate(Z137)
       deallocate(X94)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S596),size(S596),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S596,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S597(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S597)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X90),size(X90),'3124',-1.000,
     & X90,S597)
       deallocate(S597)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'612345',t3C1,F2)
       allocate(Z133(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K9*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X90,F2,Z133)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234516',
     & -1.000,V3C,Z133)
       deallocate(Z133)
       deallocate(X90)
C
       allocate(D1(N1+1:N3,N0+1:N2,M1+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S600(N0+1:M1,N0+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K2
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S600)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S600),size(S600),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,N0-N0/),'2341',S600,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S605(N2+1:M2,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S605)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X96),size(X96),'3124',-1.000,
     & X96,S605)
       deallocate(S605)
C
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N1-N1,N0-N0,M1-M1/),'621345',t3C1,F2)
       allocate(Z139(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K6
       I3=K0*K7
       call EGEMM(I1,I2,I3,X96,F2,Z139)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134526',
     & -1.000,V3C,Z139)
       deallocate(Z139)
       deallocate(X96)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S600),size(S600),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,N0-N0/),'2341',S600,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S601(M2+1:N3,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S601)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X92),size(X92),'3124',-1.000,
     & X92,S601)
       deallocate(S601)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'612345',t3C1,F2)
       allocate(Z135(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K9*K0
       I3=K0*K7
       call EGEMM(I1,I2,I3,X92,F2,Z135)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234516',
     & -1.000,V3C,Z135)
       deallocate(Z135)
       deallocate(X92)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S598(N0+1:M1,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S598)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S598),size(S598),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'2341',S598,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S604(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S604)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X95),size(X95),'3124',-1.000,
     & X95,S604)
       deallocate(S604)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N1-N1,N0-N0,M1-M1/),'621345',t3C1,F2)
       allocate(Z138(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X95,F2,Z138)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134526',
     & -1.000,V3C,Z138)
       deallocate(Z138)
       deallocate(X95)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S598),size(S598),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'2341',S598,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S599(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S599)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X91),size(X91),'3124',-1.000,
     & X91,S599)
       deallocate(S599)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'612345',t3C1,F2)
       allocate(Z134(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K9*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,X91,F2,Z134)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234516',
     & -1.000,V3C,Z134)
       deallocate(Z134)
       deallocate(X91)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S594(N0+1:M1,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S594)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S594),size(S594),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S594,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S595(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S595)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X89),size(X89),'3124',-1.000,
     & X89,S595)
       deallocate(S595)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'612345',t3C1,F2)
       allocate(Z132(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X89,F2,Z132)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234516',
     & -1.000,V3C,Z132)
       deallocate(Z132)
       deallocate(X89)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S594),size(S594),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S594,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S602(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S602)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X93),size(X93),'3124',-1.000,
     & X93,S602)
       deallocate(S602)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,M2-M2,N1-N1,N0-N0,M1-M1/),'612345',t3C3,F2)
       allocate(Z136(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X93,F2,Z136)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134526',
     & 1.000,V3C,Z136)
       deallocate(Z136)
       deallocate(X93)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q66(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q66)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(Q66),size(Q66),shape(B1),size(B1),
     & (/N1-N1,M1-M1/),'21',Q66,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(Q67(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q67)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X30),size(X30),'21',1.000,
     & X30,Q67)
       deallocate(Q67)
C
       call sumx_sorted21(N0,N1,N0,N1,
     & M1,N1,N0,M1,X30,FAHH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'612345',t3C1,F2)
       allocate(Z30(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K5*K9*K0*K6
       I3=K7
       call EGEMM(I1,I2,I3,X30,F2,Z30)
       deallocate(F2)
C
       V3C=V3C-Z30
       deallocate(Z30)
       deallocate(X30)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S606(M1+1:N2,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S606)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S606),size(S606),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'3241',S606,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S607(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S607)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X97),size(X97),'3124',1.000,
     & X97,S607)
       deallocate(S607)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N2,N2-N2,N0-N0,N0-N0/),'531246',t3C1,F2)
       allocate(Z140(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K5*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X97,F2,Z140)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124635',
     & 1.000,V3C,Z140)
       deallocate(Z140)
       deallocate(X97)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S608(M1+1:N2,M1+1:N2,N0+1:N1,N1+1:M2))
       I1=K9*K1*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S608)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S608),size(S608),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'3241',S608,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S609(N1+1:M2,M1+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S609)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X98),size(X98),'3124',1.000,
     & X98,S609)
       deallocate(S609)
C
       allocate(F2(M1+1:N2,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N1-N1,M2-N2,N2-N2,N0-N0,N0-N0/),'531246',t3C1,F2)
       allocate(Z141(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K5*K5*K0*K6
       I3=K9*K8
       call EGEMM(I1,I2,I3,X98,F2,Z141)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124635',
     & 1.000,V3C,Z141)
       deallocate(Z141)
       deallocate(X98)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q68(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q68)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(Q68),size(Q68),shape(B1),size(B1),
     & (/N2-N2,M1-M1/),'21',Q68,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q72(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q72)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X105),size(X105),'21',1.000,
     & X105,Q72)
       deallocate(Q72)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(Z148(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K8*K9*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X105,F2,Z148)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123564',
     & -1.000,V3C,Z148)
       deallocate(Z148)
       deallocate(X105)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(Q68),size(Q68),shape(B1),size(B1),
     & (/N2-N2,M1-M1/),'21',Q68,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q69(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q69)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X99),size(X99),'21',1.000,
     & X99,Q69)
       deallocate(Q69)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'512346',t3C1,F2)
       allocate(Z142(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K5*K9*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X99,F2,Z142)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123465',
     & -1.000,V3C,Z142)
       deallocate(Z142)
       deallocate(X99)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S612(N0+1:M1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S612)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S612),size(S612),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'3241',S612,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S613(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S613)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X101),size(X101),'3124',
     & 1.000,X101,S613)
       deallocate(S613)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N2,N2-N2,M1-M1,N0-N0/),'431256',t3C1,F2)
       allocate(Z144(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X101,F2,Z144)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & 1.000,V3C,Z144)
       deallocate(Z144)
       deallocate(X101)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S614(N0+1:M1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S614)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S614),size(S614),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'3241',S614,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S615(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S615)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X102),size(X102),'3124',
     & 1.000,X102,S615)
       deallocate(S615)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,N2-N2,M1-M1,N0-N0/),'431256',t3C1,F2)
       allocate(Z145(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X102,F2,Z145)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & 1.000,V3C,Z145)
       deallocate(Z145)
       deallocate(X102)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S616(N0+1:M1,M1+1:N2,N0+1:N1,N1+1:M2))
       I1=K9*K1*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S616)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S616),size(S616),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,N0-N0/),'3241',S616,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S617(N1+1:M2,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S617)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X103),size(X103),'3124',
     & 1.000,X103,S617)
       deallocate(S617)
C
       allocate(F2(M1+1:N2,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,N2-N2,M1-M1,N0-N0/),'431256',t3C1,F2)
       allocate(Z146(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K6
       I3=K9*K8
       call EGEMM(I1,I2,I3,X103,F2,Z146)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & 1.000,V3C,Z146)
       deallocate(Z146)
       deallocate(X103)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q70(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q70)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q70),size(Q70),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q70,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q71(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q71)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X104),size(X104),'21',1.000,
     & X104,Q71)
       deallocate(Q71)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(Z147(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K8*K9*K0*K6
       I3=K5
       call EGEMM(I1,I2,I3,X104,F2,Z147)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123564',
     & -1.000,V3C,Z147)
       deallocate(Z147)
       deallocate(X104)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S610(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S610)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S610),size(S610),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'3241',S610,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S611(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S611)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X100),size(X100),'3124',
     & 1.000,X100,S611)
       deallocate(S611)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N2,N2-N2,M1-M1,N0-N0/),'431256',t3C1,F2)
       allocate(Z143(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X100,F2,Z143)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & 1.000,V3C,Z143)
       deallocate(Z143)
       deallocate(X100)
C
       allocate(D1(N0+1:N1,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S618(N1+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S618)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S618),size(S618),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N1-N1/),'2341',S618,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S619(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S619)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X65),size(X65),'3124',1.000,
     & X65,S619)
       deallocate(S619)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,M2,N3,M2,N3,N1,M2,X65,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M2-N2,M2-N1,N2-N2,N0-N0,M1-M1,N0-N0/),'132456',t3C1,F2)
       allocate(Z65(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K5*K8*K5*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,X65,F2,Z65)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'245613',
     & 1.000,V3C,Z65)
       deallocate(Z65)
       deallocate(X65)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S620(N1+1:M2,N0+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S620)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S620),size(S620),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,N1-N1/),'2341',S620,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S626(N2+1:M2,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S626)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X69),size(X69),'3124',1.000,
     & X69,S626)
       deallocate(S626)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,M2,N3,N2,M2,N1,M2,X69,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N2-N2,M2-N1,M2-N2,N0-N0,M1-M1,N0-N0/),'231456',t3C1,F2)
       allocate(Z69(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K5*K8*K5*K6
       I3=K6*K0
       call EGEMM(I1,I2,I3,X69,F2,Z69)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'145623',
     & 1.000,V3C,Z69)
       deallocate(Z69)
       deallocate(X69)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S620),size(S620),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,N1-N1/),'2341',S620,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S621(M2+1:N3,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S621)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X66),size(X66),'3124',1.000,
     & X66,S621)
       deallocate(S621)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,M2,N3,M2,N3,N1,M2,X66,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N2-N2,M2-N1,N2-N2,N0-N0,M1-M1,N0-N0/),'132456',t3C1,F2)
       allocate(Z66(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K5*K8*K5*K0
       I3=K6*K0
       call EGEMM(I1,I2,I3,X66,F2,Z66)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'245613',
     & 1.000,V3C,Z66)
       deallocate(Z66)
       deallocate(X66)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S624(N1+1:M2,N0+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S624)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S624),size(S624),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N1-N1/),'2341',S624,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S628(N2+1:M2,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S628)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X71),size(X71),'3124',1.000,
     & X71,S628)
       deallocate(S628)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,N1,M2,N2,M2,N1,M2,X71,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N2-N2,N1-N1,M2-N2,N0-N0,M1-M1,N0-N0/),'231456',t3C1,F2)
       allocate(Z71(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K5*K8*K5*K6
       I3=K9*K0
       call EGEMM(I1,I2,I3,X71,F2,Z71)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'145623',
     & 1.000,V3C,Z71)
       deallocate(Z71)
       deallocate(X71)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S624),size(S624),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N1-N1/),'2341',S624,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S625(M2+1:N3,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S625)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X68),size(X68),'3124',1.000,
     & X68,S625)
       deallocate(S625)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,N1,M2,M2,N3,N1,M2,X68,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N2-N2,N1-N1,N2-N2,N0-N0,M1-M1,N0-N0/),'132456',t3C1,F2)
       allocate(Z68(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K5*K8*K5*K0
       I3=K9*K0
       call EGEMM(I1,I2,I3,X68,F2,Z68)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'245613',
     & 1.000,V3C,Z68)
       deallocate(Z68)
       deallocate(X68)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q75(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q75)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q78(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q75,B2,Q78)
       deallocate(B2)
C
       call sum_stripe(2,shape(X109),size(X109),'21',-1.000,
     & X109,Q78)
       deallocate(Q78)
C
       allocate(F2(N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N2-N2,M2-N2,N1-N1,N0-N0,M1-M1,N0-N0/),'213456',t3C1,F2)
       allocate(Z159(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K8*K5*K9*K6
       I3=K0
       call EGEMM(I1,I2,I3,X109,F2,Z159)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134562',
     & 1.000,V3C,Z159)
       deallocate(Z159)
       deallocate(X109)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q76(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q75,B2,Q76)
       deallocate(B2)
C
       call sum_stripe(2,shape(X107),size(X107),'21',-1.000,
     & X107,Q76)
       deallocate(Q76)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N1-N1,N0-N0,M1-M1,N0-N0/),'123456',t3C1,F2)
       allocate(Z154(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K8*K5*K9*K0
       I3=K0
       call EGEMM(I1,I2,I3,X107,F2,Z154)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234561',
     & 1.000,V3C,Z154)
       deallocate(Z154)
       deallocate(X107)
C
       allocate(D1(N0+1:N1,N0+1:N2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S622(N1+1:M2,N0+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S622)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S622),size(S622),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N1-N1/),'2341',S622,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S623(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S623)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X67),size(X67),'3124',1.000,
     & X67,S623)
       deallocate(S623)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,N1,M2,M2,N3,N1,M2,X67,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M2-N2,N1-N1,N2-N2,N0-N0,M1-M1,N0-N0/),'132456',t3C1,F2)
       allocate(Z67(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K5*K8*K5*K0
       I3=K9*K6
       call EGEMM(I1,I2,I3,X67,F2,Z67)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'245613',
     & 1.000,V3C,Z67)
       deallocate(Z67)
       deallocate(X67)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S622),size(S622),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N1-N1/),'2341',S622,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S627(N2+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S627)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X70),size(X70),'3124',1.000,
     & X70,S627)
       deallocate(S627)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,N1,M2,N2,M2,N1,M2,X70,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M2-M2,N1-N1,M2-M2,N0-N0,M1-M1,N0-N0/),'132456',t3C3,F2)
       allocate(Z70(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K5*K8*K5*K6
       I3=K9*K6
       call EGEMM(I1,I2,I3,X70,F2,Z70)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'145623',
     & -1.000,V3C,Z70)
       deallocate(Z70)
       deallocate(X70)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q73(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q74(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q73,B2,Q74)
       deallocate(B2)
C
       call sum_stripe(2,shape(X106),size(X106),'21',-1.000,
     & X106,Q74)
       deallocate(Q74)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N1-N1,N0-N0,M1-M1,N0-N0/),'123456',t3C1,F2)
       allocate(Z153(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K8*K5*K9*K0
       I3=K6
       call EGEMM(I1,I2,I3,X106,F2,Z153)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234561',
     & 1.000,V3C,Z153)
       deallocate(Z153)
       deallocate(X106)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q77(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q73,B2,Q77)
       deallocate(B2)
C
       call sum_stripe(2,shape(X108),size(X108),'21',-1.000,
     & X108,Q77)
       deallocate(Q77)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M2-M2,M2-M2,N1-N1,N0-N0,M1-M1,N0-N0/),'123456',t3C3,F2)
       allocate(Z158(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K8*K5*K9*K6
       I3=K6
       call EGEMM(I1,I2,I3,X108,F2,Z158)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134562',
     & -1.000,V3C,Z158)
       deallocate(Z158)
       deallocate(X108)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q79(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q79)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q80(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q79,B2,Q80)
       deallocate(B2)
C
       call sum_stripe(2,shape(X31),size(X31),'21',-1.000,
     & X31,Q80)
       deallocate(Q80)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & M2,N3,N1,M2,X31,FAPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M2-N1,M2-N2,N2-N2,N0-N0,M1-M1,N0-N0/),'312456',t3C1,F2)
       allocate(Z31(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K8*K5*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,X31,F2,Z31)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124563',
     & 1.000,V3C,Z31)
       deallocate(Z31)
       deallocate(X31)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q81(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q81)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q82(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q81,B2,Q82)
       deallocate(B2)
C
       call sum_stripe(2,shape(X32),size(X32),'21',-1.000,
     & X32,Q82)
       deallocate(Q82)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & N1,M2,N1,M2,X32,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N1-N1,M2-N2,N2-N2,N0-N0,M1-M1,N0-N0/),'312456',t3C1,F2)
       allocate(Z32(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K8*K5*K0*K6
       I3=K9
       call EGEMM(I1,I2,I3,X32,F2,Z32)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124563',
     & 1.000,V3C,Z32)
       deallocate(Z32)
       deallocate(X32)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S629(N0+1:M1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S629)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S629),size(S629),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'2431',S629,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S631(M2+1:N3,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S631)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S631)
       deallocate(S631)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S629),size(S629),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'2431',S629,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S632(M2+1:N3,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S632)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',1.000,X3,
     & S632)
       deallocate(S632)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S629),size(S629),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'2431',S629,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S633(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S633)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2314',1.000,X2,
     & S633)
       deallocate(S633)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S629),size(S629),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'2431',S629,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S630(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S630)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2314',1.000,X4,
     & S630)
       deallocate(S630)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S634(N2+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S634)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S634),size(S634),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3412',S634,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S635(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S635)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'3124',-1.000,
     & X8,S635)
       deallocate(S635)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q83(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q83)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q83),size(Q83),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q83,B1)
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,M1-N0,N0-N0/),'2134',t2B,D2)
       allocate(S639(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S639)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S639)
       deallocate(S639)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S641(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K1
       call EGEMM(I1,I2,I3,Q83,D2,S641)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S641)
       deallocate(S641)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q83),size(Q83),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q83,B1)
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,N0-N0,N0-N0/),'2134',t2B,D2)
       allocate(S643(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S643)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S643)
       deallocate(S643)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S636(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K1
       call EGEMM(I1,I2,I3,Q83,D2,S636)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S636)
       deallocate(S636)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S637(N1+1:M2,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S637)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S637),size(S637),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N1-N1/),'2341',S637,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S642(M2+1:N3,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S642)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2413',-1.000,
     & X5,S642)
       deallocate(S642)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S637),size(S637),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N1-N1/),'2341',S637,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S640(N2+1:M2,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S640)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2413',-1.000,
     & X6,S640)
       deallocate(S640)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S637),size(S637),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N1-N1/),'2341',S637,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S638(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S638)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2413',-1.000,
     & X7,S638)
       deallocate(S638)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S644(N0+1:M1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S644)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S644),size(S644),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S644,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S646(M2+1:N3,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S646)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',1.000,X3,
     & S646)
       deallocate(S646)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S644),size(S644),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'3421',S644,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S647(N1+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S647)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'2314',-1.000,
     & X13,S647)
       deallocate(S647)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S644),size(S644),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S644,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S648(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S648)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2314',1.000,X2,
     & S648)
       deallocate(S648)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S644),size(S644),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S644,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S649(M2+1:N3,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S649)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S649)
       deallocate(S649)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S644),size(S644),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'3421',S644,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S650(N1+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S650)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'2314',-1.000,
     & X14,S650)
       deallocate(S650)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S644),size(S644),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'2341',S644,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S651(M2+1:N3,N1+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S651)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X15),size(X15),'2314',1.000,
     & X15,S651)
       deallocate(S651)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S644),size(S644),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'2341',S644,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S652(N2+1:M2,N1+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S652)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X16),size(X16),'2314',1.000,
     & X16,S652)
       deallocate(S652)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S644),size(S644),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N0-N0,N0-N0/),'4231',S644,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S812(M1+1:N2,N0+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S812)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S812),size(S812),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,N0-N0/),'3214',S812,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S813(N1+1:M2,N0+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S813)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X13),size(X13),'2134',-1.000,
     & X13,S813)
       deallocate(S813)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S644),size(S644),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'3241',S644,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S816(N1+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S816)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S816),size(S816),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'2314',S816,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S818(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S818)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X16),size(X16),'2134',1.000,
     & X16,S818)
       deallocate(S818)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S644),size(S644),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N0-N0,N0-N0/),'4231',S644,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S814(N0+1:M1,N0+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S814)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S814),size(S814),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3214',S814,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S815(N1+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S815)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X14),size(X14),'2134',-1.000,
     & X14,S815)
       deallocate(S815)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S644),size(S644),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S644,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S645(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S645)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2314',1.000,X4,
     & S645)
       deallocate(S645)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S812),size(S812),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,N0-N0/),'2314',S812,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S819(M2+1:N3,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S819)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S819)
       deallocate(S819)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S812),size(S812),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,N0-N0/),'2314',S812,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S820(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S820)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S820)
       deallocate(S820)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S816),size(S816),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'2314',S816,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S817(M2+1:N3,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S817)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X15),size(X15),'2134',1.000,
     & X15,S817)
       deallocate(S817)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S814),size(S814),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S814,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S821(M2+1:N3,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S821)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',-1.000,
     & X3,S821)
       deallocate(S821)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S814),size(S814),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S814,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S822(N2+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S822)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',-1.000,
     & X4,S822)
       deallocate(S822)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S655(N1+1:M2,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S655)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S655),size(S655),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N1-N1/),'2431',S655,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S665(N2+1:M2,N0+1:M1,N2+1:N3,N1+1:M2))
       I1=K9*K4
       I2=K5*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S665)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X16),size(X16),'2413',1.000,
     & X16,S665)
       deallocate(S665)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S655),size(S655),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N1-N1/),'2341',S655,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S656(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S656)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2413',-1.000,
     & X7,S656)
       deallocate(S656)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:M2))
       call reorder_shift(4,shape(S655),size(S655),shape(D1),size(D1),
     & (/N2-N2,N1-N1,N0-N0,N1-N1/),'3421',S655,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',t2B,D2)
       allocate(S657(M1+1:N2,N0+1:M1,N0+1:N2,N1+1:M2))
       I1=K9*K2
       I2=K5*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S657)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'3412',-1.000,
     & X13,S657)
       deallocate(S657)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S655),size(S655),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N1-N1/),'2341',S655,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S659(N2+1:M2,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S659)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2413',-1.000,
     & X6,S659)
       deallocate(S659)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S655),size(S655),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N1-N1/),'2341',S655,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S660(M2+1:N3,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S660)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2413',-1.000,
     & X5,S660)
       deallocate(S660)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:M2))
       call reorder_shift(4,shape(S655),size(S655),shape(D1),size(D1),
     & (/N2-N2,N1-N1,N0-N0,N1-N1/),'3421',S655,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',t2B,D2)
       allocate(S661(N0+1:M1,N0+1:M1,N0+1:N2,N1+1:M2))
       I1=K9*K2
       I2=K5*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S661)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'3412',-1.000,
     & X14,S661)
       deallocate(S661)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S655),size(S655),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N1-N1,N1-N1/),'3241',S655,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S823(M1+1:N2,N0+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S823)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S823),size(S823),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2341',S823,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S824(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S824)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',1.000,X5,
     & S824)
       deallocate(S824)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S655),size(S655),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N1-N1,N1-N1/),'3241',S655,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S826(N0+1:M1,N0+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S826)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S826),size(S826),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2341',S826,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S827(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S827)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',1.000,X7,
     & S827)
       deallocate(S827)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S655),size(S655),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N1-N1/),'2431',S655,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S663(M2+1:N3,N0+1:M1,N2+1:N3,N1+1:M2))
       I1=K9*K4
       I2=K5*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S663)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X15),size(X15),'2413',1.000,
     & X15,S663)
       deallocate(S663)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S823),size(S823),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2341',S823,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S825(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S825)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'2134',1.000,X6,
     & S825)
       deallocate(S825)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S826),size(S826),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2341',S826,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S828(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S828)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',1.000,X8,
     & S828)
       deallocate(S828)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S674(N0+1:M1,N0+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S674)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S674),size(S674),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2341',S674,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S675(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S675)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',-1.000,
     & X7,S675)
       deallocate(S675)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S674),size(S674),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2341',S674,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S676(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S676)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',-1.000,
     & X8,S676)
       deallocate(S676)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M1-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S677(N0+1:M1,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S677)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S677),size(S677),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S677,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S678(M2+1:N3,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S678)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',-1.000,
     & X9,S678)
       deallocate(S678)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S677),size(S677),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S677,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S679(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S679)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',-1.000,
     & X10,S679)
       deallocate(S679)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N0-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S682(M1+1:N2,N0+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S682)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S682),size(S682),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2341',S682,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S684(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S684)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',1.000,
     & X10,S684)
       deallocate(S684)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S682),size(S682),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2341',S682,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S683(M2+1:N3,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S683)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',1.000,X9,
     & S683)
       deallocate(S683)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,M2-N2/),'2413',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S685(M1+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S685)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S685),size(S685),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'2341',S685,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S689(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S689)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'3124',-1.000,
     & X11,S689)
       deallocate(S689)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S685),size(S685),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M2-M2,M1-M1/),'3241',S685,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S686(N0+1:M1,N0+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S686)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'3124',1.000,X9,
     & S686)
       deallocate(S686)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'2413',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S687(M1+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S687)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S687),size(S687),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2341',S687,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S690(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S690)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'2134',1.000,
     & X11,S690)
       deallocate(S690)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S687),size(S687),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,M1-M1/),'3241',S687,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S688(N0+1:M1,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S688)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'3124',1.000,
     & X10,S688)
       deallocate(S688)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S691(N2+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S691)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S691),size(S691),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2314',S691,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S692(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S692)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'2134',1.000,
     & X12,S692)
       deallocate(S692)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S693(N0+1:M1,N0+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S693)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S693),size(S693),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,N0-N0/),'2341',S693,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S694(N2+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S694)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'3124',1.000,
     & X12,S694)
       deallocate(S694)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S695(N0+1:M1,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S695)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S695),size(S695),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2341',S695,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S696(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S696)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'2134',-1.000,
     & X12,S696)
       deallocate(S696)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S697(M1+1:N2,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S697)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S697),size(S697),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S697,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S701(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S701)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X19),size(X19),'3124',-1.000,
     & X19,S701)
       deallocate(S701)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,M1,N2,X19,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N2,N1-N1,N0-N0,N0-N0/),'621345',t3B1,F2)
       allocate(Z19(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X19,F2,Z19)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & -1.000,V3C,Z19)
       deallocate(Z19)
       deallocate(X19)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S697),size(S697),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S697,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S698(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S698)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X17),size(X17),'3124',-1.000,
     & X17,S698)
       deallocate(S698)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M1,N2,X17,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N2-N2,N1-N1,N0-N0,N0-N0/),'621345',t3B1,F2)
       allocate(Z17(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K5*K9*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X17,F2,Z17)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & 1.000,V3C,Z17)
       deallocate(Z17)
       deallocate(X17)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S699(M1+1:N2,N0+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S699)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S699),size(S699),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'2341',S699,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S702(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S702)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X20),size(X20),'3124',-1.000,
     & X20,S702)
       deallocate(S702)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,M1,N2,X20,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,N1-N1,M2-N2,N1-N1,N0-N0,N0-N0/),'621345',t3B1,F2)
       allocate(Z20(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X20,F2,Z20)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & -1.000,V3C,Z20)
       deallocate(Z20)
       deallocate(X20)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S699),size(S699),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'2341',S699,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S700(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S700)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X18),size(X18),'3124',-1.000,
     & X18,S700)
       deallocate(S700)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,M1,N2,X18,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N2-N2,N1-N1,N0-N0,N0-N0/),'621345',t3B1,F2)
       allocate(Z18(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K5*K9*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X18,F2,Z18)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & 1.000,V3C,Z18)
       deallocate(Z18)
       deallocate(X18)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S703(N0+1:M1,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S703)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S703),size(S703),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S703,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S711(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S711)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X25),size(X25),'3124',-1.000,
     & X25,S711)
       deallocate(S711)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,N0,M1,X25,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N2,N1-N1,M1-M1,N0-N0/),'521346',t3B4,F2)
       allocate(Z25(M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X25,F2,Z25)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & -1.000,V3C,Z25)
       deallocate(Z25)
       deallocate(X25)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S703),size(S703),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S703,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S704(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S704)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X21),size(X21),'3124',-1.000,
     & X21,S704)
       deallocate(S704)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,M1,X21,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N2-N2,N1-N1,M1-M1,N0-N0/),'521346',t3B4,F2)
       allocate(Z21(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K5*K8*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X21,F2,Z21)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & 1.000,V3C,Z21)
       deallocate(Z21)
       deallocate(X21)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S707(N0+1:M1,N0+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S707)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S707),size(S707),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S707,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S713(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S713)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X27),size(X27),'3124',-1.000,
     & X27,S713)
       deallocate(S713)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,N2,M2,N0,M1,X27,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,N1-N1,M1-M1,N0-N0/),'521346',t3B4,F2)
       allocate(Z27(M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X27,F2,Z27)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & -1.000,V3C,Z27)
       deallocate(Z27)
       deallocate(X27)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S707),size(S707),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S707,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S708(M2+1:N3,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S708)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X23),size(X23),'3124',-1.000,
     & X23,S708)
       deallocate(S708)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,M2,N3,N0,M1,X23,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N2-N2,N1-N1,M1-M1,N0-N0/),'521346',t3B4,F2)
       allocate(Z23(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K5*K8*K9*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,X23,F2,Z23)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & 1.000,V3C,Z23)
       deallocate(Z23)
       deallocate(X23)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S705(N0+1:M1,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S705)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S705),size(S705),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S705,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S712(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S712)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X26),size(X26),'3124',-1.000,
     & X26,S712)
       deallocate(S712)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,N0,M1,X26,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N2,N1-N1,M1-N0,N0-N0/),'621345',t3B1,F2)
       allocate(Z26(M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X26,F2,Z26)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & 1.000,V3C,Z26)
       deallocate(Z26)
       deallocate(X26)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S705),size(S705),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S705,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S706(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S706)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X22),size(X22),'3124',-1.000,
     & X22,S706)
       deallocate(S706)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,M1,X22,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N2-N2,N1-N1,M1-N0,N0-N0/),'621345',t3B1,F2)
       allocate(Z22(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K5*K8*K9*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X22,F2,Z22)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & -1.000,V3C,Z22)
       deallocate(Z22)
       deallocate(X22)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S709(N0+1:M1,N0+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S709)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S709),size(S709),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,N0-N0/),'2341',S709,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S714(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S714)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X28),size(X28),'3124',-1.000,
     & X28,S714)
       deallocate(S714)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,N0,M1,X28,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,N1-N1,M2-N2,N1-N1,M1-N0,N0-N0/),'621345',t3B1,F2)
       allocate(Z28(M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X28,F2,Z28)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & 1.000,V3C,Z28)
       deallocate(Z28)
       deallocate(X28)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S709),size(S709),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,N0-N0/),'2341',S709,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S710(M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S710)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X24),size(X24),'3124',-1.000,
     & X24,S710)
       deallocate(S710)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,N0,M1,X24,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N2-N2,N1-N1,M1-N0,N0-N0/),'621345',t3B1,F2)
       allocate(Z24(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K5*K8*K9*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X24,F2,Z24)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & -1.000,V3C,Z24)
       deallocate(Z24)
       deallocate(X24)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N2-N2/),'2341',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S715(M1+1:N2,N0+1:M1,M1+1:N2,N2+1:N3))
       I1=K4*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S715)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S715),size(S715),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M1-M1,M1-M1/),'4231',S715,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S716(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S716)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X72),size(X72),'3124',1.000,
     & X72,S716)
       deallocate(S716)
C
       call sumx_sorted3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,N0,M1,M1,N2,X72,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,N2-N2,N1-N1,N0-N0/),'451236',t3C1,F2)
       allocate(Z72(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K5*K9*K0*K6
       I3=K8*K5
       call EGEMM(I1,I2,I3,X72,F2,Z72)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123645',
     & 1.000,V3C,Z72)
       deallocate(Z72)
       deallocate(X72)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,N2-N2/),'2341',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S717(M1+1:N2,M1+1:N2,M1+1:N2,N2+1:N3))
       I1=K4*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S717)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S717),size(S717),shape(D1),size(D1),
     & (/N2-N2,M1-M1,M1-M1,M1-M1/),'4231',S717,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S718(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S718)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X73),size(X73),'3124',1.000,
     & X73,S718)
       deallocate(S718)
C
       call sumx_sorted3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,N0,M1,M1,N2,X73,VCHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,N2-N2,N1-N1,N0-N0/),'451236',t3C1,F2)
       allocate(Z73(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K5*K9*K0*K6
       I3=K8*K8
       call EGEMM(I1,I2,I3,X73,F2,Z73)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123645',
     & 0.500,V3C,Z73)
       deallocate(Z73)
       deallocate(X73)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S719(M1+1:N2,N0+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S719)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S719),size(S719),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S719,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S723(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S723)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X76),size(X76),'3124',1.000,
     & X76,S723)
       deallocate(S723)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,M1,N2,X76,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-M1,M2-M2,M2-M2,N1-N1,N0-N0,N0-N0/),'512346',t3C3,F2)
       allocate(Z76(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X76,F2,Z76)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & 1.000,V3C,Z76)
       deallocate(Z76)
       deallocate(X76)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S719),size(S719),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S719,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S720(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S720)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X74),size(X74),'3124',1.000,
     & X74,S720)
       deallocate(S720)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,M2,N3,M1,N2,X74,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'512346',t3C1,F2)
       allocate(Z74(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K5*K9*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X74,F2,Z74)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & -1.000,V3C,Z74)
       deallocate(Z74)
       deallocate(X74)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S721(M1+1:N2,N0+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S721)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S721),size(S721),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'2341',S721,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S724(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S724)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X77),size(X77),'3124',1.000,
     & X77,S724)
       deallocate(S724)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,M1,N2,X77,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N2,N1-N1,N0-N0,N0-N0/),'521346',t3C1,F2)
       allocate(Z77(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X77,F2,Z77)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & -1.000,V3C,Z77)
       deallocate(Z77)
       deallocate(X77)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S721),size(S721),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'2341',S721,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S722(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S722)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X75),size(X75),'3124',1.000,
     & X75,S722)
       deallocate(S722)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,M2,N3,M1,N2,X75,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'512346',t3C1,F2)
       allocate(Z75(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K5*K9*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,X75,F2,Z75)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & -1.000,V3C,Z75)
       deallocate(Z75)
       deallocate(X75)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q85(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q85)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(Q85),size(Q85),shape(B1),size(B1),
     & (/N2-N2,M1-M1/),'21',Q85,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q89(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q89)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X35),size(X35),'21',1.000,
     & X35,Q89)
       deallocate(Q89)
C
       call sumx_sorted21(N0,N2,N0,N2,
     & M1,N2,N0,M1,X35,FBHH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(Z35(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K8*K9*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X35,F2,Z35)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123564',
     & -1.000,V3C,Z35)
       deallocate(Z35)
       deallocate(X35)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(Q85),size(Q85),shape(B1),size(B1),
     & (/N2-N2,M1-M1/),'21',Q85,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q86(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q86)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X33),size(X33),'21',1.000,
     & X33,Q86)
       deallocate(Q86)
C
       call sumx_sorted21(N0,N2,N0,N2,
     & M1,N2,M1,N2,X33,FBHH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'512346',t3C1,F2)
       allocate(Z33(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K5*K9*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X33,F2,Z33)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123465',
     & -1.000,V3C,Z33)
       deallocate(Z33)
       deallocate(X33)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S727(N0+1:M1,N0+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S727)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S727),size(S727),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S727,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S734(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S734)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X83),size(X83),'3124',1.000,
     & X83,S734)
       deallocate(S734)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,M1,X83,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,M2-M2,N1-N1,M1-M1,N0-N0/),'412356',t3C3,F2)
       allocate(Z83(M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X83,F2,Z83)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & 1.000,V3C,Z83)
       deallocate(Z83)
       deallocate(X83)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S727),size(S727),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S727,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S728(M2+1:N3,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S728)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X79),size(X79),'3124',1.000,
     & X79,S728)
       deallocate(S728)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,M1,X79,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(Z79(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K5*K8*K9*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X79,F2,Z79)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & -1.000,V3C,Z79)
       deallocate(Z79)
       deallocate(X79)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S731(N0+1:M1,N0+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S731)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S731),size(S731),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,N0-N0/),'2341',S731,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S736(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S736)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X85),size(X85),'3124',1.000,
     & X85,S736)
       deallocate(S736)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,M1,X85,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N1-N1,M1-M1,N0-N0/),'421356',t3C1,F2)
       allocate(Z85(M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X85,F2,Z85)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & -1.000,V3C,Z85)
       deallocate(Z85)
       deallocate(X85)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S731),size(S731),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,N0-N0/),'2341',S731,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S732(M2+1:N3,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S732)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X81),size(X81),'3124',1.000,
     & X81,S732)
       deallocate(S732)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,M2,N3,N0,M1,X81,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(Z81(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K5*K8*K9*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,X81,F2,Z81)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & -1.000,V3C,Z81)
       deallocate(Z81)
       deallocate(X81)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S729(N0+1:M1,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S729)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S729),size(S729),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'2341',S729,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S735(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S735)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X84),size(X84),'3124',1.000,
     & X84,S735)
       deallocate(S735)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,M1,X84,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N1-N1,M1-M1,N0-N0/),'421356',t3C1,F2)
       allocate(Z84(M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X84,F2,Z84)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & -1.000,V3C,Z84)
       deallocate(Z84)
       deallocate(X84)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S729),size(S729),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'2341',S729,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S730(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S730)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X80),size(X80),'3124',1.000,
     & X80,S730)
       deallocate(S730)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,M2,N3,N0,M1,X80,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(Z80(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K5*K8*K9*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,X80,F2,Z80)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & -1.000,V3C,Z80)
       deallocate(Z80)
       deallocate(X80)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S725(N0+1:M1,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S725)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S725),size(S725),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S725,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S726(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S726)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X78),size(X78),'3124',1.000,
     & X78,S726)
       deallocate(S726)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,M1,X78,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(Z78(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K5*K8*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X78,F2,Z78)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & -1.000,V3C,Z78)
       deallocate(Z78)
       deallocate(X78)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S725),size(S725),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S725,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S733(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S733)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X82),size(X82),'3124',1.000,
     & X82,S733)
       deallocate(S733)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,M1,X82,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,M2-M2,N1-N1,M1-M1,N0-N0/),'412356',t3C3,F2)
       allocate(Z82(M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X82,F2,Z82)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & 1.000,V3C,Z82)
       deallocate(Z82)
       deallocate(X82)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q87(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q87)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q87),size(Q87),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q87,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q88(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q88)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X34),size(X34),'21',1.000,
     & X34,Q88)
       deallocate(Q88)
C
       call sumx_sorted21(N0,N2,N0,N2,
     & N0,M1,N0,M1,X34,FBHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N1-N1,M1-M1,N0-N0/),'412356',t3C1,F2)
       allocate(Z34(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K8*K9*K0*K6
       I3=K5
       call EGEMM(I1,I2,I3,X34,F2,Z34)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123564',
     & -1.000,V3C,Z34)
       deallocate(Z34)
       deallocate(X34)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S737(N2+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S737)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S737),size(S737),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N2-N2/),'2341',S737,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S738(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S738)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X86),size(X86),'3124',1.000,
     & X86,S738)
       deallocate(S738)
C
       call sumx_sorted1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,M2,N3,M2,N3,N2,M2,X86,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M2-M2,M2-M2,N1-N1,N0-N0,M1-M1,N0-N0/),'123456',t3C3,F2)
       allocate(Z86(N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K5*K8*K5*K9
       I3=K6*K6
       call EGEMM(I1,I2,I3,X86,F2,Z86)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'345612',
     & 0.500,V3C,Z86)
       deallocate(Z86)
       deallocate(X86)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S739(N2+1:M2,N0+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S739)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S739),size(S739),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,N2-N2/),'2341',S739,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S740(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S740)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X87),size(X87),'3124',1.000,
     & X87,S740)
       deallocate(S740)
C
       call sumx_sorted1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,N2,M2,M2,N3,N2,M2,X87,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N1-N1,N0-N0,M1-M1,N0-N0/),'123456',t3C1,F2)
       allocate(Z87(N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K5*K8*K5*K9
       I3=K0*K6
       call EGEMM(I1,I2,I3,X87,F2,Z87)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'345612',
     & 1.000,V3C,Z87)
       deallocate(Z87)
       deallocate(X87)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S741(N2+1:M2,N0+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S741)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S741),size(S741),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2341',S741,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S742(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S742)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X88),size(X88),'3124',1.000,
     & X88,S742)
       deallocate(S742)
C
       call sumx_sorted1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,M2,N2,M2,M2,N3,N2,M2,X88,VCAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N1-N1,N0-N0,M1-M1,N0-N0/),'123456',t3C1,F2)
       allocate(Z88(N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K5*K8*K5*K9
       I3=K0*K0
       call EGEMM(I1,I2,I3,X88,F2,Z88)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'345612',
     & 0.500,V3C,Z88)
       deallocate(Z88)
       deallocate(X88)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q90(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q90)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q94(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q90,B2,Q94)
       deallocate(B2)
C
       call sum_stripe(2,shape(X38),size(X38),'21',-1.000,
     & X38,Q94)
       deallocate(Q94)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & M2,N3,N2,M2,X38,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M2-M2,M2-M2,N1-N1,N0-N0,M1-M1,N0-N0/),'123456',t3C3,F2)
       allocate(Z38(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K8*K5*K9*K6
       I3=K6
       call EGEMM(I1,I2,I3,X38,F2,Z38)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134562',
     & -1.000,V3C,Z38)
       deallocate(Z38)
       deallocate(X38)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q91(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q90,B2,Q91)
       deallocate(B2)
C
       call sum_stripe(2,shape(X36),size(X36),'21',-1.000,
     & X36,Q91)
       deallocate(Q91)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & M2,N3,M2,N3,X36,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N1-N1,N0-N0,M1-M1,N0-N0/),'123456',t3C1,F2)
       allocate(Z36(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K8*K5*K9*K0
       I3=K6
       call EGEMM(I1,I2,I3,X36,F2,Z36)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234561',
     & 1.000,V3C,Z36)
       deallocate(Z36)
       deallocate(X36)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q92(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q92)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q95(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q92,B2,Q95)
       deallocate(B2)
C
       call sum_stripe(2,shape(X39),size(X39),'21',-1.000,
     & X39,Q95)
       deallocate(Q95)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & N2,M2,N2,M2,X39,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N2-N2,M2-N2,N1-N1,N0-N0,M1-M1,N0-N0/),'213456',t3C1,F2)
       allocate(Z39(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K8*K5*K9*K6
       I3=K0
       call EGEMM(I1,I2,I3,X39,F2,Z39)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134562',
     & 1.000,V3C,Z39)
       deallocate(Z39)
       deallocate(X39)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q93(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q92,B2,Q93)
       deallocate(B2)
C
       call sum_stripe(2,shape(X37),size(X37),'21',-1.000,
     & X37,Q93)
       deallocate(Q93)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & N2,M2,M2,N3,X37,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N1-N1,N0-N0,M1-M1,N0-N0/),'123456',t3C1,F2)
       allocate(Z37(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K8*K5*K9*K0
       I3=K0
       call EGEMM(I1,I2,I3,X37,F2,Z37)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234561',
     & 1.000,V3C,Z37)
       deallocate(Z37)
       deallocate(X37)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S743(M1+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S743)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S743),size(S743),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'2341',S743,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S750(N2+1:M2,N1+1:M2,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S750)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2314',1.000,X6,
     & S750)
       deallocate(S750)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S743),size(S743),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'2341',S743,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S751(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S751)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2314',1.000,X5,
     & S751)
       deallocate(S751)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S743),size(S743),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'3421',S743,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S752(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S752)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2314',1.000,
     & X10,S752)
       deallocate(S752)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,M1+1:N2))
       call reorder_shift(4,shape(S743),size(S743),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'2431',S743,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S753(M2+1:N3,N0+1:M1,N0+1:N1,M1+1:N2))
       I1=K8*K1
       I2=K5*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S753)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2413',-1.000,
     & X1,S753)
       deallocate(S753)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S743),size(S743),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'3421',S743,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S754(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S754)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2314',1.000,X9,
     & S754)
       deallocate(S754)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,M1+1:N2))
       call reorder_shift(4,shape(S743),size(S743),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'2431',S743,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S755(N2+1:M2,N0+1:M1,N0+1:N1,M1+1:N2))
       I1=K8*K1
       I2=K5*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S755)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2413',-1.000,
     & X2,S755)
       deallocate(S755)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S743),size(S743),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'3421',S743,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S744(N1+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S744)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'2413',1.000,
     & X13,S744)
       deallocate(S744)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S745(N0+1:M1,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S745)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S745),size(S745),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S745,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S756(N2+1:M2,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S756)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2314',1.000,X8,
     & S756)
       deallocate(S756)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S745),size(S745),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S745,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S757(M2+1:N3,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S757)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2314',1.000,X7,
     & S757)
       deallocate(S757)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S745),size(S745),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'3421',S745,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S770(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S770)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2413',-1.000,
     & X10,S770)
       deallocate(S770)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S745),size(S745),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'2431',S745,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S771(M2+1:N3,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S771)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2413',-1.000,
     & X3,S771)
       deallocate(S771)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S745),size(S745),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'3421',S745,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S772(M2+1:N3,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S772)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2413',-1.000,
     & X9,S772)
       deallocate(S772)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S745),size(S745),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'2431',S745,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3214',t2B,D2)
       allocate(S773(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S773)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2413',-1.000,
     & X4,S773)
       deallocate(S773)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S745),size(S745),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'3421',S745,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S746(N1+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S746)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'2413',1.000,
     & X14,S746)
       deallocate(S746)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',t2B,D2)
       allocate(S762(M1+1:N2,N0+1:M1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K5*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S762)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S762),size(S762),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,N0-N0/),'3412',S762,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S768(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S768)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S768)
       deallocate(S768)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S762),size(S762),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M1-M1,N0-N0/),'3412',S762,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S763(M2+1:N3,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S763)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S763)
       deallocate(S763)
C
       call sumx_sorted2134(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M2,N3,M1,N2,N0,M1,X1,VBHHHP, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(Z1(N2+1:M2,N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N2,N0+1:M1))
       I1=K5*K8*K6
       I2=K5*K9*K0
       I3=K1
       call EGEMM(I1,I2,I3,X1,D2,Z1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234156',
     & 1.000,V3C,Z1)
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q96(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q96)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q96),size(Q96),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q96,B1)
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,M1-N0,N0-N0/),'2134',t2B,D2)
       allocate(S769(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S769)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S769)
       deallocate(S769)
C
       call sumx_sorted2134(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,M2,M1,N2,N0,M1,X2,VBHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(Z2(M2+1:N3,N1+1:M2,N0+1:M1,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K0
       I2=K5*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X2,D2,Z2)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134256',
     & -1.000,V3C,Z2)
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S781(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K1
       call EGEMM(I1,I2,I3,Q96,D2,S781)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S781)
       deallocate(S781)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q96),size(Q96),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q96,B1)
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,N0-N0,N0-N0/),'2134',t2B,D2)
       allocate(S784(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S784)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S784)
       deallocate(S784)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S765(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K1
       call EGEMM(I1,I2,I3,Q96,D2,S765)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S765)
       deallocate(S765)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S747(N1+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S747)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S747),size(S747),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3412',S747,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S749(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S749)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X16),size(X16),'2134',-1.000,
     & X16,S749)
       deallocate(S749)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S747),size(S747),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3412',S747,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S748(M2+1:N3,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S748)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X15),size(X15),'2134',-1.000,
     & X15,S748)
       deallocate(S748)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S758(N1+1:M2,N0+1:M1,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S758)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S758),size(S758),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3412',S758,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S764(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S764)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',1.000,X8,
     & S764)
       deallocate(S764)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S758),size(S758),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3412',S758,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S759(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S759)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',1.000,X7,
     & S759)
       deallocate(S759)
C
       call sumx_sorted2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M2,N3,N1,M2,N0,M1,X7,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,M1-N0,N0-N0/),'2134',t2B,D2)
       allocate(Z7(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K6
       I2=K5*K8*K0
       I3=K3
       call EGEMM(I1,I2,I3,X7,D2,Z7)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'256134',
     & 1.000,V3C,Z7)
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S774(N1+1:M2,M1+1:N2,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S774)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S774),size(S774),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'3412',S774,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S775(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S775)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',1.000,X5,
     & S775)
       deallocate(S775)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S776(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S776)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S776),size(S776),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S776,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S777(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S777)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'2134',-1.000,
     & X11,S777)
       deallocate(S777)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',t2B,D2)
       allocate(S778(N0+1:M1,N0+1:M1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K5*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S778)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S778),size(S778),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3412',S778,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S779(M2+1:N3,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S779)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',-1.000,
     & X3,S779)
       deallocate(S779)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S766(N2+1:M2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S766)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S766),size(S766),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N2-N2/),'2431',S766,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S782(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S782)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'2413',1.000,
     & X11,S782)
       deallocate(S782)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:M2))
       call reorder_shift(4,shape(S766),size(S766),shape(D1),size(D1),
     & (/N2-N2,N1-N1,N0-N0,N2-N2/),'3421',S766,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',t2B,D2)
       allocate(S783(N0+1:M1,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1
       I2=K5*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S783)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'3412',-1.000,
     & X4,S783)
       deallocate(S783)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S766),size(S766),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N2-N2/),'2431',S766,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S767(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S767)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'2413',1.000,
     & X12,S767)
       deallocate(S767)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S766),size(S766),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N2-N2/),'2341',S766,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(S780(N1+1:M2,M1+1:N2,N1+1:N3,N2+1:M2))
       I1=K0*K3
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S780)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'3412',1.000,X6,
     & S780)
       deallocate(S780)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S760(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S760)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S760),size(S760),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S760,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S761(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S761)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'2134',-1.000,
     & X12,S761)
       deallocate(S761)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S785(M1+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S785)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S785),size(S785),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S785,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S787(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S787)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2314',-1.000,
     & X10,S787)
       deallocate(S787)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S785),size(S785),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S785,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S788(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S788)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2314',-1.000,
     & X9,S788)
       deallocate(S788)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S785),size(S785),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'3421',S785,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S789(N1+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S789)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'2413',1.000,
     & X13,S789)
       deallocate(S789)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S785),size(S785),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N0-N0,M1-M1/),'4231',S785,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S829(N0+1:M1,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S829)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S829),size(S829),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S829,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S830(M2+1:N3,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S830)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',1.000,X9,
     & S830)
       deallocate(S830)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S785),size(S785),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'3241',S785,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S832(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S832)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S832),size(S832),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2314',S832,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S833(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S833)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'2134',-1.000,
     & X11,S833)
       deallocate(S833)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S785),size(S785),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'2341',S785,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S786(M2+1:N3,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S786)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'2314',-0.500,
     & X11,S786)
       deallocate(S786)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S829),size(S829),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S829,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S831(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S831)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',1.000,
     & X10,S831)
       deallocate(S831)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q97(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q97)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q97),size(Q97),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q97,B1)
       allocate(D2(N2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',t2B,D2)
       allocate(S803(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S803)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'2341',1.000,
     & X14,S803)
       deallocate(S803)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q97),size(Q97),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q97,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S809(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S809)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S809)
       deallocate(S809)
C
       allocate(D2(N0+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S811(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K2
       call EGEMM(I1,I2,I3,Q97,D2,S811)
       deallocate(D2)
C
       call sum_stripe(4,shape(X16),size(X16),'2341',-1.000,
     & X16,S811)
       deallocate(S811)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q97),size(Q97),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q97,B1)
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',t2B,D2)
       allocate(S796(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S796)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',1.000,
     & X13,S796)
       deallocate(S796)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4132',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S800(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S800)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S800),size(S800),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S800,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S801(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S801)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'2134',1.000,
     & X11,S801)
       deallocate(S801)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S794(N2+1:M2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S794)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(S794),size(S794),shape(D1),size(D1),
     & (/N2-N2,N2-N2,N0-N0,N2-N2/),'3421',S794,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S808(N0+1:M1,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S808)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'3412',0.500,
     & X10,S808)
       deallocate(S808)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S794),size(S794),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2431',S794,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S810(N1+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S810)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X16),size(X16),'3412',1.000,
     & X16,S810)
       deallocate(S810)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S794),size(S794),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2341',S794,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S795(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S795)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'2413',1.000,
     & X12,S795)
       deallocate(S795)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S794),size(S794),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2341',S794,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S802(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S802)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'2413',1.000,
     & X11,S802)
       deallocate(S802)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S790(N0+1:M1,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S790)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S790),size(S790),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S790,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S798(M2+1:N3,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S798)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2413',1.000,X9,
     & S798)
       deallocate(S798)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S790),size(S790),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'3421',S790,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S799(N1+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S799)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'2413',1.000,
     & X14,S799)
       deallocate(S799)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S790),size(S790),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'2341',S790,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S791(M2+1:N3,N2+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S791)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'2314',-0.500,
     & X12,S791)
       deallocate(S791)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S790),size(S790),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'3241',S790,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S834(N2+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S834)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S834),size(S834),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2314',S834,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S835(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S835)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'2134',-1.000,
     & X12,S835)
       deallocate(S835)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S790),size(S790),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S790,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S797(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S797)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2413',1.000,
     & X10,S797)
       deallocate(S797)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q84(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q84)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D2(N0+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S666(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K2
       call EGEMM(I1,I2,I3,Q84,D2,S666)
       deallocate(D2)
C
       call sum_stripe(4,shape(X16),size(X16),'2341',-1.000,
     & X16,S666)
       deallocate(S666)
C
       call sumx_sorted2314(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N2,M2,N1,M2,N0,M1,X16,VBHPPP, 1.000)
C
       allocate(D2(N2+1:N3,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,M2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z16(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K0
       I2=K8*K5*K6
       I3=K4
       call EGEMM(I1,I2,I3,X16,D2,Z16)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'145236',
     & -1.000,V3C,Z16)
       deallocate(Z16)
       deallocate(X16)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q84),size(Q84),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q84,B1)
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',t2B,D2)
       allocate(S658(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S658)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'2341',1.000,
     & X13,S658)
       deallocate(S658)
C
       call sumx_sorted1234(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,M2,M1,N2,N0,M1,X13,VBHHPH, 1.000)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(Z13(M2+1:N3,N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K9
       I2=K5*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X13,D2,Z13)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124356',
     & 1.000,V3C,Z13)
       deallocate(Z13)
       deallocate(X13)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q84),size(Q84),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q84,B1)
       allocate(D2(N2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',t2B,D2)
       allocate(S662(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S662)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'2341',1.000,
     & X14,S662)
       deallocate(S662)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q84),size(Q84),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q84,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S664(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S664)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S664)
       deallocate(S664)
C
       call sumx_sorted2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,M2,N0,M1,M1,N2,X10,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(Z10(M2+1:N3,N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K5*K9*K6
       I3=K2
       call EGEMM(I1,I2,I3,X10,D2,Z10)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'136245',
     & 1.000,V3C,Z10)
       deallocate(Z10)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q64(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q64)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q64),size(Q64),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q64,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(Q65(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q65)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X29),size(X29),'21',1.000,
     & X29,Q65)
       deallocate(Q65)
C
       call sumx_sorted21(N0,N1,N0,N1,
     & N0,M1,N0,M1,X29,FAHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'612345',t3C1,F2)
       allocate(Z29(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K5*K9*K0*K6
       I3=K5
       call EGEMM(I1,I2,I3,X29,F2,Z29)
       deallocate(F2)
C
       V3C=V3C-Z29
       deallocate(Z29)
       deallocate(X29)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N0-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S573(N0+1:M1,N0+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S573)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S573),size(S573),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3214',S573,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S574(N1+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S574)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X14),size(X14),'2134',-1.000,
     & X14,S574)
       deallocate(S574)
C
       call sumx_sorted1234(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,M2,N0,M1,N0,M1,X14,VBHHPH, 1.000)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z14(M2+1:N3,N2+1:M2,M1+1:N2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K9
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X14,D2,Z14)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'125346',
     & -1.000,V3C,Z14)
       deallocate(Z14)
       deallocate(X14)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S573),size(S573),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S573,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S670(N2+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S670)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',-1.000,
     & X4,S670)
       deallocate(S670)
C
       call sumx_sorted2134(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,M2,N0,M1,N0,M1,X4,VBHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(Z4(M2+1:N3,N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K8*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X4,D2,Z4)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'135246',
     & 1.000,V3C,Z4)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S573),size(S573),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S573,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S669(M2+1:N3,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S669)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',-1.000,
     & X3,S669)
       deallocate(S669)
C
       call sumx_sorted2134(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M2,N3,N0,M1,N0,M1,X3,VBHHHP, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(Z3(N2+1:M2,N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5*K5*K6
       I2=K8*K9*K0
       I3=K1
       call EGEMM(I1,I2,I3,X3,D2,Z3)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'235146',
     & -1.000,V3C,Z3)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S671(M1+1:N2,N0+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S671)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S671),size(S671),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2341',S671,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S673(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S673)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'2134',-1.000,
     & X6,S673)
       deallocate(S673)
C
       call sumx_sorted2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N2,M2,N1,M2,M1,N2,X6,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,M2-N2,N0-N0,N0-N0/),'2134',t2B,D2)
       allocate(Z6(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K0
       I2=K5*K5*K6
       I3=K3
       call EGEMM(I1,I2,I3,X6,D2,Z6)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'146235',
     & 1.000,V3C,Z6)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S671),size(S671),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2341',S671,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S672(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S672)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',-1.000,
     & X5,S672)
       deallocate(S672)
C
       call sumx_sorted2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M2,N3,N1,M2,M1,N2,X5,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,N0-N0,N0-N0/),'2134',t2B,D2)
       allocate(Z5(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K6
       I2=K5*K5*K0
       I3=K3
       call EGEMM(I1,I2,I3,X5,D2,Z5)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'246135',
     & -1.000,V3C,Z5)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S680(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S680)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S680),size(S680),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2314',S680,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S681(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S681)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'2134',1.000,
     & X11,S681)
       deallocate(S681)
C
       call sumx_sorted2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M2,N3,N2,M2,M1,N2,X11,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',t2B,D2)
       allocate(Z11(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K5*K5*K9
       I3=K4
       call EGEMM(I1,I2,I3,X11,D2,Z11)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'346125',
     & 1.000,V3C,Z11)
       deallocate(Z11)
       deallocate(X11)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S804(N0+1:M1,M1+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S804)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S804),size(S804),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3412',S804,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S805(M2+1:N3,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S805)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',-0.500,
     & X9,S805)
       deallocate(S805)
C
       call sumx_sorted2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M2,N3,N0,M1,M1,N2,X9,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(Z9(N2+1:M2,N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1,M1+1:N2))
       I1=K8*K5*K6
       I2=K5*K9*K0
       I3=K2
       call EGEMM(I1,I2,I3,X9,D2,Z9)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'236145',
     & -1.000,V3C,Z9)
       deallocate(Z9)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4132',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S792(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S792)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S792),size(S792),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S792,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S793(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S793)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'2134',1.000,
     & X12,S793)
       deallocate(S793)
C
       call sumx_sorted2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M2,N3,N2,M2,N0,M1,X12,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',t2B,D2)
       allocate(Z12(N1+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K6
       I2=K5*K8*K9
       I3=K4
       call EGEMM(I1,I2,I3,X12,D2,Z12)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'356124',
     & -1.000,V3C,Z12)
       deallocate(Z12)
       deallocate(X12)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S806(N1+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S806)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S806),size(S806),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3412',S806,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S807(M2+1:N3,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S807)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X15),size(X15),'2134',-1.000,
     & X15,S807)
       deallocate(S807)
C
       call sumx_sorted2314(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M2,N3,N1,M2,N0,M1,X15,VBHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z15(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K6
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X15,D2,Z15)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'245136',
     & 1.000,V3C,Z15)
       deallocate(Z15)
       deallocate(X15)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S653(N2+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S653)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S653),size(S653),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3412',S653,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S654(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S654)
       deallocate(D1)
       deallocate(B2)
       deallocate(S653)
C
       call sum_stripe(4,shape(X8),size(X8),'3124',-1.000,
     & X8,S654)
       deallocate(S654)
C
       call sumx_sorted2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N2,M2,N1,M2,N0,M1,X8,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,M2-N2,M1-N0,N0-N0/),'2134',t2B,D2)
       allocate(Z8(M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K0
       I2=K5*K8*K6
       I3=K3
       call EGEMM(I1,I2,I3,X8,D2,Z8)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'156234',
     & -1.000,V3C,Z8)
       deallocate(Z8)
       deallocate(X8)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'1324',VBHPPH,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z89(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z89)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z89)
       deallocate(Z89)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'1324',VBHPPH,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z90(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z90)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z90)
       deallocate(Z90)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'1324',VBHPPH,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'421356',t3D,F2)
       allocate(Z91(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z91)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z91)
       deallocate(Z91)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'1324',VBHPPH,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'521346',t3D,F2)
       allocate(Z92(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z92)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z92)
       deallocate(Z92)
C
       call sumx_sorted3(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,HT3C1,V3C,1.0)
       deallocate(V3C)
C
       end
