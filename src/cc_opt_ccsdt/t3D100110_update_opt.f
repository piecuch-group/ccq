       subroutine t3D100110_update(N0,N1,N2,N3,HT3D,shift,
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
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q9(:,:)
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
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
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
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::S70(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S72(:,:,:,:)
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::S74(:,:,:,:)
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S76(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::Q19(:,:)
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
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::Q21(:,:)
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
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::S119(:,:,:,:)
       real*8,allocatable::S120(:,:,:,:)
       real*8,allocatable::S121(:,:,:,:)
       real*8,allocatable::S122(:,:,:,:)
       real*8,allocatable::S123(:,:,:,:)
       real*8,allocatable::S124(:,:,:,:)
       real*8,allocatable::S125(:,:,:,:)
       real*8,allocatable::S126(:,:,:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::S127(:,:,:,:)
       real*8,allocatable::S128(:,:,:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::Q27(:,:)
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
       real*8,allocatable::S162(:,:,:,:)
       real*8,allocatable::S163(:,:,:,:)
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
       real*8,allocatable::Q28(:,:)
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
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q36(:,:)
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
       real*8,allocatable::Q37(:,:)
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
       real*8,allocatable::Q38(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::S350(:,:,:,:)
       real*8,allocatable::S351(:,:,:,:)
       real*8,allocatable::S352(:,:,:,:)
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::S353(:,:,:,:)
       real*8,allocatable::S354(:,:,:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q44(:,:)
       real*8,allocatable::Q45(:,:)
       real*8,allocatable::Q46(:,:)
       real*8,allocatable::Q50(:,:)
       real*8,allocatable::Q47(:,:)
       real*8,allocatable::Q48(:,:)
       real*8,allocatable::Q49(:,:)
       real*8,allocatable::Q51(:,:)
       real*8,allocatable::Q55(:,:)
       real*8,allocatable::Q57(:,:)
       real*8,allocatable::Q52(:,:)
       real*8,allocatable::Q53(:,:)
       real*8,allocatable::Q54(:,:)
       real*8,allocatable::Q58(:,:)
       real*8,allocatable::Q56(:,:)
       real*8,allocatable::Q59(:,:)
       real*8,allocatable::S356(:,:,:,:)
       real*8,allocatable::S357(:,:,:,:)
       real*8,allocatable::S358(:,:,:,:)
       real*8,allocatable::S355(:,:,:,:)
       real*8,allocatable::S359(:,:,:,:)
       real*8,allocatable::S361(:,:,:,:)
       real*8,allocatable::S362(:,:,:,:)
       real*8,allocatable::S360(:,:,:,:)
       real*8,allocatable::S363(:,:,:,:)
       real*8,allocatable::S367(:,:,:,:)
       real*8,allocatable::S364(:,:,:,:)
       real*8,allocatable::S365(:,:,:,:)
       real*8,allocatable::S366(:,:,:,:)
       real*8,allocatable::S368(:,:,:,:)
       real*8,allocatable::S370(:,:,:,:)
       real*8,allocatable::S371(:,:,:,:)
       real*8,allocatable::S369(:,:,:,:)
       real*8,allocatable::S372(:,:,:,:)
       real*8,allocatable::S378(:,:,:,:)
       real*8,allocatable::S379(:,:,:,:)
       real*8,allocatable::S373(:,:,:,:)
       real*8,allocatable::S374(:,:,:,:)
       real*8,allocatable::S380(:,:,:,:)
       real*8,allocatable::S382(:,:,:,:)
       real*8,allocatable::S375(:,:,:,:)
       real*8,allocatable::S376(:,:,:,:)
       real*8,allocatable::S377(:,:,:,:)
       real*8,allocatable::S383(:,:,:,:)
       real*8,allocatable::S381(:,:,:,:)
       real*8,allocatable::S384(:,:,:,:)
       real*8,allocatable::S386(:,:,:,:)
       real*8,allocatable::S387(:,:,:,:)
       real*8,allocatable::S385(:,:,:,:)
       real*8,allocatable::S388(:,:,:,:)
       real*8,allocatable::S392(:,:,:,:)
       real*8,allocatable::S389(:,:,:,:)
       real*8,allocatable::S390(:,:,:,:)
       real*8,allocatable::S391(:,:,:,:)
       real*8,allocatable::S393(:,:,:,:)
       real*8,allocatable::S399(:,:,:,:)
       real*8,allocatable::S400(:,:,:,:)
       real*8,allocatable::S394(:,:,:,:)
       real*8,allocatable::S395(:,:,:,:)
       real*8,allocatable::S401(:,:,:,:)
       real*8,allocatable::S403(:,:,:,:)
       real*8,allocatable::S396(:,:,:,:)
       real*8,allocatable::S397(:,:,:,:)
       real*8,allocatable::S398(:,:,:,:)
       real*8,allocatable::S404(:,:,:,:)
       real*8,allocatable::S402(:,:,:,:)
       real*8,allocatable::S405(:,:,:,:)
       real*8,allocatable::S409(:,:,:,:)
       real*8,allocatable::S411(:,:,:,:)
       real*8,allocatable::S406(:,:,:,:)
       real*8,allocatable::S407(:,:,:,:)
       real*8,allocatable::S408(:,:,:,:)
       real*8,allocatable::S412(:,:,:,:)
       real*8,allocatable::S410(:,:,:,:)
       real*8,allocatable::S413(:,:,:,:)
       real*8,allocatable::S421(:,:,:,:)
       real*8,allocatable::S425(:,:,:,:)
       real*8,allocatable::S414(:,:,:,:)
       real*8,allocatable::S419(:,:,:,:)
       real*8,allocatable::S424(:,:,:,:)
       real*8,allocatable::S428(:,:,:,:)
       real*8,allocatable::S420(:,:,:,:)
       real*8,allocatable::S417(:,:,:,:)
       real*8,allocatable::S418(:,:,:,:)
       real*8,allocatable::S427(:,:,:,:)
       real*8,allocatable::S423(:,:,:,:)
       real*8,allocatable::S415(:,:,:,:)
       real*8,allocatable::S416(:,:,:,:)
       real*8,allocatable::S422(:,:,:,:)
       real*8,allocatable::S426(:,:,:,:)
       real*8,allocatable::S429(:,:,:,:)
       real*8,allocatable::S430(:,:,:,:)
       real*8,allocatable::S431(:,:,:,:)
       real*8,allocatable::S432(:,:,:,:)
       real*8,allocatable::S433(:,:,:,:)
       real*8,allocatable::S437(:,:,:,:)
       real*8,allocatable::S434(:,:,:,:)
       real*8,allocatable::S435(:,:,:,:)
       real*8,allocatable::S438(:,:,:,:)
       real*8,allocatable::S436(:,:,:,:)
       real*8,allocatable::S439(:,:,:,:)
       real*8,allocatable::S440(:,:,:,:)
       real*8,allocatable::S441(:,:,:,:)
       real*8,allocatable::S442(:,:,:,:)
       real*8,allocatable::Q60(:,:)
       real*8,allocatable::Q64(:,:)
       real*8,allocatable::Q61(:,:)
       real*8,allocatable::S445(:,:,:,:)
       real*8,allocatable::S446(:,:,:,:)
       real*8,allocatable::S447(:,:,:,:)
       real*8,allocatable::S448(:,:,:,:)
       real*8,allocatable::S449(:,:,:,:)
       real*8,allocatable::S457(:,:,:,:)
       real*8,allocatable::S450(:,:,:,:)
       real*8,allocatable::S453(:,:,:,:)
       real*8,allocatable::S459(:,:,:,:)
       real*8,allocatable::S454(:,:,:,:)
       real*8,allocatable::S451(:,:,:,:)
       real*8,allocatable::S458(:,:,:,:)
       real*8,allocatable::S452(:,:,:,:)
       real*8,allocatable::S455(:,:,:,:)
       real*8,allocatable::S460(:,:,:,:)
       real*8,allocatable::S456(:,:,:,:)
       real*8,allocatable::S461(:,:,:,:)
       real*8,allocatable::S462(:,:,:,:)
       real*8,allocatable::S463(:,:,:,:)
       real*8,allocatable::S464(:,:,:,:)
       real*8,allocatable::S465(:,:,:,:)
       real*8,allocatable::S466(:,:,:,:)
       real*8,allocatable::S467(:,:,:,:)
       real*8,allocatable::S468(:,:,:,:)
       real*8,allocatable::Q62(:,:)
       real*8,allocatable::Q63(:,:)
       real*8,allocatable::S443(:,:,:,:)
       real*8,allocatable::S444(:,:,:,:)
       real*8,allocatable::S469(:,:,:,:)
       real*8,allocatable::S470(:,:,:,:)
       real*8,allocatable::S471(:,:,:,:)
       real*8,allocatable::S481(:,:,:,:)
       real*8,allocatable::S472(:,:,:,:)
       real*8,allocatable::S475(:,:,:,:)
       real*8,allocatable::S476(:,:,:,:)
       real*8,allocatable::S477(:,:,:,:)
       real*8,allocatable::S478(:,:,:,:)
       real*8,allocatable::S479(:,:,:,:)
       real*8,allocatable::S480(:,:,:,:)
       real*8,allocatable::Q65(:,:)
       real*8,allocatable::Q69(:,:)
       real*8,allocatable::Q71(:,:)
       real*8,allocatable::Q66(:,:)
       real*8,allocatable::S473(:,:,:,:)
       real*8,allocatable::S474(:,:,:,:)
       real*8,allocatable::S482(:,:,:,:)
       real*8,allocatable::Q67(:,:)
       real*8,allocatable::Q68(:,:)
       real*8,allocatable::Q72(:,:)
       real*8,allocatable::Q70(:,:)
       real*8,allocatable::S483(:,:,:,:)
       real*8,allocatable::S485(:,:,:,:)
       real*8,allocatable::S486(:,:,:,:)
       real*8,allocatable::S484(:,:,:,:)
       real*8,allocatable::S487(:,:,:,:)
       real*8,allocatable::S489(:,:,:,:)
       real*8,allocatable::S490(:,:,:,:)
       real*8,allocatable::S501(:,:,:,:)
       real*8,allocatable::S502(:,:,:,:)
       real*8,allocatable::S503(:,:,:,:)
       real*8,allocatable::S488(:,:,:,:)
       real*8,allocatable::S497(:,:,:,:)
       real*8,allocatable::S509(:,:,:,:)
       real*8,allocatable::S511(:,:,:,:)
       real*8,allocatable::S498(:,:,:,:)
       real*8,allocatable::S491(:,:,:,:)
       real*8,allocatable::S500(:,:,:,:)
       real*8,allocatable::S492(:,:,:,:)
       real*8,allocatable::S495(:,:,:,:)
       real*8,allocatable::S508(:,:,:,:)
       real*8,allocatable::S510(:,:,:,:)
       real*8,allocatable::S496(:,:,:,:)
       real*8,allocatable::S504(:,:,:,:)
       real*8,allocatable::S505(:,:,:,:)
       real*8,allocatable::S493(:,:,:,:)
       real*8,allocatable::S494(:,:,:,:)
       real*8,allocatable::S499(:,:,:,:)
       real*8,allocatable::S506(:,:,:,:)
       real*8,allocatable::S507(:,:,:,:)
       real*8,allocatable::S512(:,:,:,:)
       real*8,allocatable::S514(:,:,:,:)
       real*8,allocatable::S515(:,:,:,:)
       real*8,allocatable::S516(:,:,:,:)
       real*8,allocatable::S517(:,:,:,:)
       real*8,allocatable::S555(:,:,:,:)
       real*8,allocatable::S556(:,:,:,:)
       real*8,allocatable::S559(:,:,:,:)
       real*8,allocatable::S560(:,:,:,:)
       real*8,allocatable::S561(:,:,:,:)
       real*8,allocatable::S562(:,:,:,:)
       real*8,allocatable::S513(:,:,:,:)
       real*8,allocatable::S557(:,:,:,:)
       real*8,allocatable::S558(:,:,:,:)
       real*8,allocatable::S563(:,:,:,:)
       real*8,allocatable::S518(:,:,:,:)
       real*8,allocatable::S526(:,:,:,:)
       real*8,allocatable::S519(:,:,:,:)
       real*8,allocatable::S520(:,:,:,:)
       real*8,allocatable::S521(:,:,:,:)
       real*8,allocatable::S522(:,:,:,:)
       real*8,allocatable::S523(:,:,:,:)
       real*8,allocatable::S524(:,:,:,:)
       real*8,allocatable::S564(:,:,:,:)
       real*8,allocatable::S565(:,:,:,:)
       real*8,allocatable::S568(:,:,:,:)
       real*8,allocatable::S569(:,:,:,:)
       real*8,allocatable::S570(:,:,:,:)
       real*8,allocatable::S571(:,:,:,:)
       real*8,allocatable::S525(:,:,:,:)
       real*8,allocatable::S566(:,:,:,:)
       real*8,allocatable::S567(:,:,:,:)
       real*8,allocatable::S572(:,:,:,:)
       real*8,allocatable::S544(:,:,:,:)
       real*8,allocatable::S546(:,:,:,:)
       real*8,allocatable::S545(:,:,:,:)
       real*8,allocatable::S535(:,:,:,:)
       real*8,allocatable::S548(:,:,:,:)
       real*8,allocatable::S552(:,:,:,:)
       real*8,allocatable::S536(:,:,:,:)
       real*8,allocatable::S547(:,:,:,:)
       real*8,allocatable::S538(:,:,:,:)
       real*8,allocatable::S549(:,:,:,:)
       real*8,allocatable::S553(:,:,:,:)
       real*8,allocatable::S539(:,:,:,:)
       real*8,allocatable::S551(:,:,:,:)
       real*8,allocatable::S527(:,:,:,:)
       real*8,allocatable::S529(:,:,:,:)
       real*8,allocatable::S532(:,:,:,:)
       real*8,allocatable::S533(:,:,:,:)
       real*8,allocatable::S528(:,:,:,:)
       real*8,allocatable::S530(:,:,:,:)
       real*8,allocatable::S537(:,:,:,:)
       real*8,allocatable::S541(:,:,:,:)
       real*8,allocatable::S531(:,:,:,:)
       real*8,allocatable::Q73(:,:)
       real*8,allocatable::S554(:,:,:,:)
       real*8,allocatable::S540(:,:,:,:)
       real*8,allocatable::S550(:,:,:,:)
       real*8,allocatable::S534(:,:,:,:)
       real*8,allocatable::S542(:,:,:,:)
       real*8,allocatable::S543(:,:,:,:)
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
       real*8,allocatable::X21(:,:)
       real*8,allocatable::Z21(:,:,:,:,:,:)
       real*8,allocatable::X22(:,:)
       real*8,allocatable::Z22(:,:,:,:,:,:)
       real*8,allocatable::X23(:,:)
       real*8,allocatable::Z23(:,:,:,:,:,:)
       real*8,allocatable::X24(:,:)
       real*8,allocatable::Z24(:,:,:,:,:,:)
       real*8,allocatable::X25(:,:)
       real*8,allocatable::Z25(:,:,:,:,:,:)
       real*8,allocatable::X26(:,:)
       real*8,allocatable::Z26(:,:,:,:,:,:)
       real*8,allocatable::X27(:,:)
       real*8,allocatable::Z27(:,:,:,:,:,:)
       real*8,allocatable::X28(:,:)
       real*8,allocatable::Z28(:,:,:,:,:,:)
       real*8,allocatable::X29(:,:)
       real*8,allocatable::Z29(:,:,:,:,:,:)
       real*8,allocatable::X30(:,:,:,:)
       real*8,allocatable::Z30(:,:,:,:,:,:)
       real*8,allocatable::X31(:,:,:,:)
       real*8,allocatable::Z31(:,:,:,:,:,:)
       real*8,allocatable::X32(:,:,:,:)
       real*8,allocatable::Z32(:,:,:,:,:,:)
       real*8,allocatable::X33(:,:,:,:)
       real*8,allocatable::Z33(:,:,:,:,:,:)
       real*8,allocatable::Z34(:,:,:,:,:,:)
       real*8,allocatable::Z35(:,:,:,:,:,:)
       real*8,allocatable::X34(:,:,:,:)
       real*8,allocatable::Z36(:,:,:,:,:,:)
       real*8,allocatable::X35(:,:,:,:)
       real*8,allocatable::Z37(:,:,:,:,:,:)
       real*8,allocatable::X36(:,:,:,:)
       real*8,allocatable::Z38(:,:,:,:,:,:)
       real*8,allocatable::X37(:,:,:,:)
       real*8,allocatable::Z39(:,:,:,:,:,:)
       real*8,allocatable::X38(:,:,:,:)
       real*8,allocatable::Z40(:,:,:,:,:,:)
       real*8,allocatable::X39(:,:,:,:)
       real*8,allocatable::Z41(:,:,:,:,:,:)
       real*8,allocatable::X40(:,:,:,:)
       real*8,allocatable::Z42(:,:,:,:,:,:)
       real*8,allocatable::Z43(:,:,:,:,:,:)
       real*8,allocatable::Z44(:,:,:,:,:,:)
       real*8,allocatable::Z45(:,:,:,:,:,:)
       real*8,allocatable::Z46(:,:,:,:,:,:)
       real*8,allocatable::X41(:,:,:,:)
       real*8,allocatable::Z47(:,:,:,:,:,:)
       real*8,allocatable::X42(:,:,:,:)
       real*8,allocatable::Z48(:,:,:,:,:,:)
       real*8,allocatable::X43(:,:,:,:)
       real*8,allocatable::Z49(:,:,:,:,:,:)
       real*8,allocatable::X44(:,:,:,:)
       real*8,allocatable::Z50(:,:,:,:,:,:)
       real*8,allocatable::X45(:,:,:,:)
       real*8,allocatable::Z51(:,:,:,:,:,:)
       real*8,allocatable::X46(:,:)
       real*8,allocatable::Z52(:,:,:,:,:,:)
       real*8,allocatable::X47(:,:)
       real*8,allocatable::Z53(:,:,:,:,:,:)
       real*8,allocatable::X48(:,:)
       real*8,allocatable::Z54(:,:,:,:,:,:)
       real*8,allocatable::X49(:,:)
       real*8,allocatable::Z55(:,:,:,:,:,:)
       real*8,allocatable::X50(:,:)
       real*8,allocatable::Z56(:,:,:,:,:,:)
       real*8,allocatable::X51(:,:)
       real*8,allocatable::Z57(:,:,:,:,:,:)
       real*8,allocatable::X52(:,:)
       real*8,allocatable::Z58(:,:,:,:,:,:)
       real*8,allocatable::X53(:,:)
       real*8,allocatable::Z59(:,:,:,:,:,:)
       real*8,allocatable::X54(:,:)
       real*8,allocatable::Z60(:,:,:,:,:,:)
       real*8,allocatable::X55(:,:,:,:)
       real*8,allocatable::Z62(:,:,:,:,:,:)
       real*8,allocatable::X56(:,:,:,:)
       real*8,allocatable::Z63(:,:,:,:,:,:)
       real*8,allocatable::X57(:,:,:,:)
       real*8,allocatable::Z67(:,:,:,:,:,:)
       real*8,allocatable::X58(:,:,:,:)
       real*8,allocatable::Z68(:,:,:,:,:,:)
       real*8,allocatable::X59(:,:,:,:)
       real*8,allocatable::Z80(:,:,:,:,:,:)
       real*8,allocatable::X60(:,:,:,:)
       real*8,allocatable::Z81(:,:,:,:,:,:)
       real*8,allocatable::X61(:,:,:,:)
       real*8,allocatable::Z82(:,:,:,:,:,:)
       real*8,allocatable::X62(:,:,:,:)
       real*8,allocatable::Z83(:,:,:,:,:,:)
       real*8,allocatable::X63(:,:,:,:)
       real*8,allocatable::Z84(:,:,:,:,:,:)
       real*8,allocatable::X64(:,:,:,:)
       real*8,allocatable::Z85(:,:,:,:,:,:)
       real*8,allocatable::X65(:,:,:,:)
       real*8,allocatable::Z86(:,:,:,:,:,:)
       real*8,allocatable::X66(:,:,:,:)
       real*8,allocatable::Z96(:,:,:,:,:,:)
       real*8,allocatable::X67(:,:,:,:)
       real*8,allocatable::Z97(:,:,:,:,:,:)
       real*8,allocatable::X68(:,:,:,:)
       real*8,allocatable::Z98(:,:,:,:,:,:)
       real*8,allocatable::X69(:,:,:,:)
       real*8,allocatable::Z99(:,:,:,:,:,:)
       real*8,allocatable::X70(:,:,:,:)
       real*8,allocatable::Z110(:,:,:,:,:,:)
       real*8,allocatable::X71(:,:,:,:)
       real*8,allocatable::Z111(:,:,:,:,:,:)
       real*8,allocatable::X72(:,:,:,:)
       real*8,allocatable::Z112(:,:,:,:,:,:)
       real*8,allocatable::X73(:,:,:,:)
       real*8,allocatable::Z113(:,:,:,:,:,:)
       real*8,allocatable::X74(:,:,:,:)
       real*8,allocatable::Z114(:,:,:,:,:,:)
       real*8,allocatable::X75(:,:,:,:)
       real*8,allocatable::Z115(:,:,:,:,:,:)
       real*8,allocatable::X76(:,:,:,:)
       real*8,allocatable::Z116(:,:,:,:,:,:)
       real*8,allocatable::X77(:,:,:,:)
       real*8,allocatable::Z117(:,:,:,:,:,:)
       real*8,allocatable::X78(:,:,:,:)
       real*8,allocatable::Z143(:,:,:,:,:,:)
       real*8,allocatable::X79(:,:,:,:)
       real*8,allocatable::Z144(:,:,:,:,:,:)
       real*8,allocatable::X80(:,:,:,:)
       real*8,allocatable::Z145(:,:,:,:,:,:)
       real*8,allocatable::X81(:,:,:,:)
       real*8,allocatable::Z146(:,:,:,:,:,:)
       real*8,allocatable::Z154(:,:,:,:,:,:)
       real*8,allocatable::Z155(:,:,:,:,:,:)
       real*8,allocatable::Z156(:,:,:,:,:,:)
       real*8,allocatable::Z157(:,:,:,:,:,:)
       real*8,allocatable::Z158(:,:,:,:,:,:)
       real*8,allocatable::X82(:,:,:,:)
       real*8,allocatable::Z163(:,:,:,:,:,:)
       real*8,allocatable::X83(:,:,:,:)
       real*8,allocatable::Z164(:,:,:,:,:,:)
       real*8,allocatable::X84(:,:,:,:)
       real*8,allocatable::Z165(:,:,:,:,:,:)
       real*8,allocatable::X85(:,:,:,:)
       real*8,allocatable::Z166(:,:,:,:,:,:)
       real*8,allocatable::X86(:,:,:,:)
       real*8,allocatable::Z167(:,:,:,:,:,:)
       real*8,allocatable::X87(:,:,:,:)
       real*8,allocatable::Z168(:,:,:,:,:,:)
       real*8,allocatable::X88(:,:,:,:)
       real*8,allocatable::Z169(:,:,:,:,:,:)
       real*8,allocatable::X89(:,:,:,:)
       real*8,allocatable::Z170(:,:,:,:,:,:)
       real*8,allocatable::Z181(:,:,:,:,:,:)
       real*8,allocatable::Z182(:,:,:,:,:,:)
       real*8,allocatable::Z183(:,:,:,:,:,:)
       real*8,allocatable::Z184(:,:,:,:,:,:)
       real*8,allocatable::X90(:,:,:,:)
       real*8,allocatable::Z185(:,:,:,:,:,:)
       real*8,allocatable::X91(:,:,:,:)
       real*8,allocatable::Z186(:,:,:,:,:,:)
       real*8,allocatable::X92(:,:,:,:)
       real*8,allocatable::Z187(:,:,:,:,:,:)
       real*8,allocatable::X93(:,:,:,:)
       real*8,allocatable::Z188(:,:,:,:,:,:)
       real*8,allocatable::X94(:,:,:,:)
       real*8,allocatable::Z189(:,:,:,:,:,:)
       real*8,allocatable::X95(:,:,:,:)
       real*8,allocatable::Z190(:,:,:,:,:,:)
       real*8,allocatable::Z228(:,:,:,:,:,:)
       real*8,allocatable::Z229(:,:,:,:,:,:)
       real*8,allocatable::Z230(:,:,:,:,:,:)
       real*8,allocatable::Z231(:,:,:,:,:,:)
       real*8,allocatable::Z232(:,:,:,:,:,:)
       real*8,allocatable::Z233(:,:,:,:,:,:)
       real*8,allocatable::Z234(:,:,:,:,:,:)
       real*8,allocatable::Z235(:,:,:,:,:,:)
       real*8,allocatable::Z236(:,:,:,:,:,:)
       real*8,allocatable::Z237(:,:,:,:,:,:)
       real*8,allocatable::Z238(:,:,:,:,:,:)
       real*8,allocatable::Z239(:,:,:,:,:,:)
       real*8,allocatable::Z240(:,:,:,:,:,:)
       real*8,allocatable::Z241(:,:,:,:,:,:)
       real*8,allocatable::Z242(:,:,:,:,:,:)
       real*8,allocatable::Z243(:,:,:,:,:,:)
       real*8,allocatable::Z244(:,:,:,:,:,:)
       real*8,allocatable::Z245(:,:,:,:,:,:)
       real*8,allocatable::Z246(:,:,:,:,:,:)
       real*8,allocatable::Z247(:,:,:,:,:,:)
       real*8,allocatable::Z248(:,:,:,:,:,:)
       real*8,allocatable::Z249(:,:,:,:,:,:)
       real*8,allocatable::X96(:,:,:,:)
       real*8,allocatable::Z250(:,:,:,:,:,:)
       real*8,allocatable::X97(:,:,:,:)
       real*8,allocatable::Z251(:,:,:,:,:,:)
       real*8,allocatable::Z253(:,:,:,:,:,:)
       real*8,allocatable::Z254(:,:,:,:,:,:)
       real*8,allocatable::Z255(:,:,:,:,:,:)
       real*8,allocatable::Z256(:,:,:,:,:,:)
       real*8,allocatable::Z257(:,:,:,:,:,:)
       real*8,allocatable::Z258(:,:,:,:,:,:)
       real*8,allocatable::Z259(:,:,:,:,:,:)
       real*8,allocatable::Z260(:,:,:,:,:,:)
       real*8,allocatable::X98(:,:,:,:)
       real*8,allocatable::Z261(:,:,:,:,:,:)
       real*8,allocatable::X99(:,:,:,:)
       real*8,allocatable::Z262(:,:,:,:,:,:)
       real*8,allocatable::X100(:,:,:,:)
       real*8,allocatable::Z263(:,:,:,:,:,:)
       real*8,allocatable::X101(:,:,:,:)
       real*8,allocatable::Z264(:,:,:,:,:,:)
       real*8,allocatable::X102(:,:,:,:)
       real*8,allocatable::Z280(:,:,:,:,:,:)
       real*8,allocatable::X103(:,:,:,:)
       real*8,allocatable::Z281(:,:,:,:,:,:)
       real*8,allocatable::X104(:,:,:,:)
       real*8,allocatable::Z284(:,:,:,:,:,:)
       real*8,allocatable::Z285(:,:,:,:,:,:)
       real*8,allocatable::Z291(:,:,:,:,:,:)
       real*8,allocatable::Z298(:,:,:,:,:,:)
       real*8,allocatable::X105(:,:,:,:)
       real*8,allocatable::Z311(:,:,:,:,:,:)
C
       allocate(V3D(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       V3D=0.0d0
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q1(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X46(M1+1:N2,M1+1:N2))
       X46=0.0d0
       X46=X46+Q1
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q2(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X47(N0+1:M1,N0+1:M1))
       X47=0.0d0
       X47=X47+Q2
       deallocate(Q2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q3(M1+1:N2,N0+1:M1))
       I1=K5*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X48(M1+1:N2,N0+1:M1))
       X48=0.0d0
       X48=X48+Q3
       deallocate(Q3)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N2,M2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q4(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X49(M2+1:N3,M2+1:N3))
       X49=0.0d0
       X49=X49+Q4
       deallocate(Q4)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,M2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q5(N2+1:M2,M2+1:N3))
       I1=K6*K0
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X50(N2+1:M2,M2+1:N3))
       X50=0.0d0
       X50=X50+Q5
       deallocate(Q5)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q6(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X51(M2+1:N3,N2+1:M2))
       X51=0.0d0
       X51=X51+Q6
       deallocate(Q6)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q7(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X52(N2+1:M2,N2+1:M2))
       X52=0.0d0
       X52=X52+Q7
       deallocate(Q7)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q8(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X53(M2+1:N3,N2+1:M2))
       X53=0.0d0
       X53=X53+Q8
       deallocate(Q8)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q9(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X54(N2+1:M2,N2+1:M2))
       X54=0.0d0
       X54=X54+Q9
       deallocate(Q9)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VCHHHH),size(VCHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,M1-N0/),'3412',VCHHHH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S1(M2+1:N3,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N2,M2+1:N3,N0+1:M1,M1+1:N2))
       X1=0.0d0
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S1)
       deallocate(S1)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VCHHHH),size(VCHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,M1-N0/),'3412',VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S2(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X55(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       X55=0.0d0
       call sum_stripe(4,shape(X55),size(X55),'2134',1.000,
     & X55,S2)
       deallocate(S2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VCHHHH),size(VCHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,M1-N0/),'4312',VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S3(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X56(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       X56=0.0d0
       call sum_stripe(4,shape(X56),size(X56),'2134',1.000,
     & X56,S3)
       deallocate(S3)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,M1-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S4(N0+1:M1,N0+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'3124',1.000,X1,
     & S4)
       deallocate(S4)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,M1-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S5(N0+1:M1,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X55),size(X55),'3124',-1.000,
     & X55,S5)
       deallocate(S5)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,M1-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S6(N0+1:M1,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X56),size(X56),'3124',1.000,
     & X56,S6)
       deallocate(S6)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S7(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X57(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X57=0.0d0
       call sum_stripe(4,shape(X57),size(X57),'3124',1.000,
     & X57,S7)
       deallocate(S7)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S8(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X58(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X58=0.0d0
       call sum_stripe(4,shape(X58),size(X58),'3124',1.000,
     & X58,S8)
       deallocate(S8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S9(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X57),size(X57),'2134',-1.000,
     & X57,S9)
       deallocate(S9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S10(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X58),size(X58),'2134',-1.000,
     & X58,S10)
       deallocate(S10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S11(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       X4=0.0d0
       call sum_stripe(4,shape(X4),size(X4),'3124',-1.000,
     & X4,S11)
       deallocate(S11)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S12(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',1.000,X4,
     & S12)
       deallocate(S12)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,N0-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S13(M1+1:N2,N0+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'4123',-1.000,
     & X1,S13)
       deallocate(S13)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N0-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S14(M1+1:N2,N0+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X55),size(X55),'4123',1.000,
     & X55,S14)
       deallocate(S14)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N0-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S15(M1+1:N2,N0+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X56),size(X56),'4123',-1.000,
     & X56,S15)
       deallocate(S15)
C
       allocate(D1(N2+1:N3,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCAPPP),size(VCAPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,M2-N2,N2-N2/),'1234',VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S16(M1+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K4
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X57),size(X57),'4123',1.000,
     & X57,S16)
       deallocate(S16)
C
       allocate(D1(N2+1:N3,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCAPPP),size(VCAPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,M2-N2,N2-N2/),'1234',VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S17(M1+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K4
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X58),size(X58),'4123',1.000,
     & X58,S17)
       deallocate(S17)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCAPPP),size(VCAPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N2-N2,N2-N2/),'1234',VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S18(M1+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K4
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'4123',-1.000,
     & X4,S18)
       deallocate(S18)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VCHHHH),size(VCHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,N0-N0/),'3412',VCHHHH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S19(M2+1:N3,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:N2,M2+1:N3,N0+1:M1,N0+1:M1))
       X5=0.0d0
       call sum_stripe(4,shape(X5),size(X5),'2134',-1.000,
     & X5,S19)
       deallocate(S19)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VCHHHH),size(VCHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,N0-N0/),'3412',VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S20(N2+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X59(N0+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       X59=0.0d0
       call sum_stripe(4,shape(X59),size(X59),'2134',1.000,
     & X59,S20)
       deallocate(S20)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VCHHHH),size(VCHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,N0-N0/),'4312',VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S21(N2+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X60(N0+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       X60=0.0d0
       call sum_stripe(4,shape(X60),size(X60),'2134',1.000,
     & X60,S21)
       deallocate(S21)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,N0-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S22(N0+1:M1,N0+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X61(N0+1:N2,M2+1:N3,N0+1:M1,N0+1:M1))
       X61=0.0d0
       call sum_stripe(4,shape(X61),size(X61),'3124',1.000,
     & X61,S22)
       deallocate(S22)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N0-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S23(N0+1:M1,N0+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X62(N0+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       X62=0.0d0
       call sum_stripe(4,shape(X62),size(X62),'3124',1.000,
     & X62,S23)
       deallocate(S23)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N0-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S24(N0+1:M1,N0+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X63(N0+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       X63=0.0d0
       call sum_stripe(4,shape(X63),size(X63),'3124',1.000,
     & X63,S24)
       deallocate(S24)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,N0-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S25(N2+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X64(N2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       X64=0.0d0
       call sum_stripe(4,shape(X64),size(X64),'3124',1.000,
     & X64,S25)
       deallocate(S25)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,N0-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S26(N2+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X65(N2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       X65=0.0d0
       call sum_stripe(4,shape(X65),size(X65),'3124',1.000,
     & X65,S26)
       deallocate(S26)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S27(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X64),size(X64),'2134',-1.000,
     & X64,S27)
       deallocate(S27)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S28(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X65),size(X65),'2134',-1.000,
     & X65,S28)
       deallocate(S28)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S29(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       X8=0.0d0
       call sum_stripe(4,shape(X8),size(X8),'3124',-1.000,
     & X8,S29)
       deallocate(S29)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S30(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',1.000,X8,
     & S30)
       deallocate(S30)
C
       allocate(D1(N2+1:N3,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCAPPP),size(VCAPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,M2-N2,N2-N2/),'1234',VCAPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S31(N0+1:M1,N2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K4
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X64),size(X64),'4123',1.000,
     & X64,S31)
       deallocate(S31)
C
       allocate(D1(N2+1:N3,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCAPPP),size(VCAPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,M2-N2,N2-N2/),'1234',VCAPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S32(N0+1:M1,N2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K4
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X65),size(X65),'4123',1.000,
     & X65,S32)
       deallocate(S32)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCAPPP),size(VCAPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N2-N2,N2-N2/),'1234',VCAPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S33(N0+1:M1,N2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K4
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'4123',-1.000,
     & X8,S33)
       deallocate(S33)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S34(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N2))
       X9=0.0d0
       call sum_stripe(4,shape(X9),size(X9),'3124',-1.000,
     & X9,S34)
       deallocate(S34)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S35(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(M1+1:N1,N1+1:M2,M2+1:N3,M1+1:N2))
       X10=0.0d0
       call sum_stripe(4,shape(X10),size(X10),'3124',-1.000,
     & X10,S35)
       deallocate(S35)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S36(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X66(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X66=0.0d0
       call sum_stripe(4,shape(X66),size(X66),'3124',1.000,
     & X66,S36)
       deallocate(S36)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S37(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X67(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X67=0.0d0
       call sum_stripe(4,shape(X67),size(X67),'3124',1.000,
     & X67,S37)
       deallocate(S37)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S38(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X68(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X68=0.0d0
       call sum_stripe(4,shape(X68),size(X68),'3124',1.000,
     & X68,S38)
       deallocate(S38)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S39(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X69(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X69=0.0d0
       call sum_stripe(4,shape(X69),size(X69),'3124',1.000,
     & X69,S39)
       deallocate(S39)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S40(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'4123',1.000,X9,
     & S40)
       deallocate(S40)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S41(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'4123',1.000,
     & X10,S41)
       deallocate(S41)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S42(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X66),size(X66),'4123',-1.000,
     & X66,S42)
       deallocate(S42)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S43(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X67),size(X67),'4123',-1.000,
     & X67,S43)
       deallocate(S43)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S44(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X68),size(X68),'4123',-1.000,
     & X68,S44)
       deallocate(S44)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S45(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X69),size(X69),'4123',-1.000,
     & X69,S45)
       deallocate(S45)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S46(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X13=0.0d0
       call sum_stripe(4,shape(X13),size(X13),'3124',-1.000,
     & X13,S46)
       deallocate(S46)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S47(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       X14=0.0d0
       call sum_stripe(4,shape(X14),size(X14),'3124',-1.000,
     & X14,S47)
       deallocate(S47)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S48(M2+1:N3,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N0+1:M1,N1+1:M2,M2+1:N3,N0+1:M1))
       X15=0.0d0
       call sum_stripe(4,shape(X15),size(X15),'3124',-1.000,
     & X15,S48)
       deallocate(S48)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S49(M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(M1+1:N1,N1+1:M2,M2+1:N3,N0+1:M1))
       X16=0.0d0
       call sum_stripe(4,shape(X16),size(X16),'3124',-1.000,
     & X16,S49)
       deallocate(S49)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S50(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X70(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X70=0.0d0
       call sum_stripe(4,shape(X70),size(X70),'3124',1.000,
     & X70,S50)
       deallocate(S50)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S51(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X71(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X71=0.0d0
       call sum_stripe(4,shape(X71),size(X71),'3124',1.000,
     & X71,S51)
       deallocate(S51)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S52(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X72(N0+1:M1,N1+1:M2,N2+1:M2,N0+1:M1))
       X72=0.0d0
       call sum_stripe(4,shape(X72),size(X72),'3124',1.000,
     & X72,S52)
       deallocate(S52)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S53(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X73(M1+1:N1,N1+1:M2,N2+1:M2,N0+1:M1))
       X73=0.0d0
       call sum_stripe(4,shape(X73),size(X73),'3124',1.000,
     & X73,S53)
       deallocate(S53)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S54(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X74(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X74=0.0d0
       call sum_stripe(4,shape(X74),size(X74),'3124',1.000,
     & X74,S54)
       deallocate(S54)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S55(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X75(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X75=0.0d0
       call sum_stripe(4,shape(X75),size(X75),'3124',1.000,
     & X75,S55)
       deallocate(S55)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S56(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X76(N0+1:M1,N1+1:M2,N2+1:M2,N0+1:M1))
       X76=0.0d0
       call sum_stripe(4,shape(X76),size(X76),'3124',1.000,
     & X76,S56)
       deallocate(S56)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S57(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X77(M1+1:N1,N1+1:M2,N2+1:M2,N0+1:M1))
       X77=0.0d0
       call sum_stripe(4,shape(X77),size(X77),'3124',1.000,
     & X77,S57)
       deallocate(S57)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S58(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X13),size(X13),'4123',1.000,
     & X13,S58)
       deallocate(S58)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S59(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X14),size(X14),'4123',1.000,
     & X14,S59)
       deallocate(S59)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S60(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X15),size(X15),'4123',1.000,
     & X15,S60)
       deallocate(S60)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,M2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S61(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X16),size(X16),'4123',1.000,
     & X16,S61)
       deallocate(S61)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S62(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S62)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X70),size(X70),'4123',-1.000,
     & X70,S62)
       deallocate(S62)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S63(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X71),size(X71),'4123',-1.000,
     & X71,S63)
       deallocate(S63)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S64(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X72),size(X72),'4123',-1.000,
     & X72,S64)
       deallocate(S64)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S65(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X73),size(X73),'4123',-1.000,
     & X73,S65)
       deallocate(S65)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S66(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S66)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X74),size(X74),'4123',-1.000,
     & X74,S66)
       deallocate(S66)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S67(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X75),size(X75),'4123',-1.000,
     & X75,S67)
       deallocate(S67)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S68(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S68)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X76),size(X76),'4123',-1.000,
     & X76,S68)
       deallocate(S68)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S69(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X77),size(X77),'4123',-1.000,
     & X77,S69)
       deallocate(S69)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,M1-N0/),'12',FBHP,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q10(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q10)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X21(M1+1:N2,M1+1:N2))
       X21=0.0d0
       call sum_stripe(2,shape(X21),size(X21),'21',1.000,
     & X21,Q10)
       deallocate(Q10)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q11(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q11)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X22(N0+1:M1,N0+1:M1))
       X22=0.0d0
       call sum_stripe(2,shape(X22),size(X22),'21',1.000,
     & X22,Q11)
       deallocate(Q11)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,M1-N0/),'12',FBHP,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q12(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q12)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X23(M1+1:N2,N0+1:M1))
       X23=0.0d0
       call sum_stripe(2,shape(X23),size(X23),'21',1.000,
     & X23,Q12)
       deallocate(Q12)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,M2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q13(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q13)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X24(M2+1:N3,M2+1:N3))
       X24=0.0d0
       call sum_stripe(2,shape(X24),size(X24),'21',-1.000,
     & X24,Q13)
       deallocate(Q13)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,N2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q14(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q14)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X25(N2+1:M2,M2+1:N3))
       X25=0.0d0
       call sum_stripe(2,shape(X25),size(X25),'21',-1.000,
     & X25,Q14)
       deallocate(Q14)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,M2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q15(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q15)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X26(M2+1:N3,N2+1:M2))
       X26=0.0d0
       call sum_stripe(2,shape(X26),size(X26),'21',-1.000,
     & X26,Q15)
       deallocate(Q15)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,N2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q16(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q16)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X27(N2+1:M2,N2+1:M2))
       X27=0.0d0
       call sum_stripe(2,shape(X27),size(X27),'21',-1.000,
     & X27,Q16)
       deallocate(Q16)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,M2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q17(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q17)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X28(M2+1:N3,N2+1:M2))
       X28=0.0d0
       call sum_stripe(2,shape(X28),size(X28),'21',-1.000,
     & X28,Q17)
       deallocate(Q17)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,N2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q18(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q18)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X29(N2+1:M2,N2+1:M2))
       X29=0.0d0
       call sum_stripe(2,shape(X29),size(X29),'21',-1.000,
     & X29,Q18)
       deallocate(Q18)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M1-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S70(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S70)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X30(N0+1:M1,M1+1:N2,N0+1:M1,M1+1:N2))
       X30=0.0d0
       call sum_stripe(4,shape(X30),size(X30),'3124',1.000,
     & X30,S70)
       deallocate(S70)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,M1-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S71(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X31(M1+1:N2,M1+1:N2,N0+1:M1,M1+1:N2))
       X31=0.0d0
       call sum_stripe(4,shape(X31),size(X31),'3124',1.000,
     & X31,S71)
       deallocate(S71)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S72(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S72)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X32(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       X32=0.0d0
       call sum_stripe(4,shape(X32),size(X32),'3124',-1.000,
     & X32,S72)
       deallocate(S72)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S73(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X33(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N2))
       X33=0.0d0
       call sum_stripe(4,shape(X33),size(X33),'3124',-1.000,
     & X33,S73)
       deallocate(S73)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S74(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S74)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X78(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X78=0.0d0
       call sum_stripe(4,shape(X78),size(X78),'3124',1.000,
     & X78,S74)
       deallocate(S74)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S75(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X79(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X79=0.0d0
       call sum_stripe(4,shape(X79),size(X79),'3124',1.000,
     & X79,S75)
       deallocate(S75)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S76(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S76)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X80(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X80=0.0d0
       call sum_stripe(4,shape(X80),size(X80),'3124',1.000,
     & X80,S76)
       deallocate(S76)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S77(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X81(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X81=0.0d0
       call sum_stripe(4,shape(X81),size(X81),'3124',1.000,
     & X81,S77)
       deallocate(S77)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M1-N0/),'3142',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q19(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q19)
       deallocate(D1)
       deallocate(B2)
C
       X21=X21+Q19
       deallocate(Q19)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N0-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S78(M1+1:N2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S78)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X30),size(X30),'4123',-1.000,
     & X30,S78)
       deallocate(S78)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,N0-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S79(M1+1:N2,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X31),size(X31),'4123',-1.000,
     & X31,S79)
       deallocate(S79)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S80(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S80)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X32),size(X32),'4123',-1.000,
     & X32,S80)
       deallocate(S80)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S81(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S81)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X33),size(X33),'4123',-1.000,
     & X33,S81)
       deallocate(S81)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S82(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S82)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X78),size(X78),'4123',1.000,
     & X78,S82)
       deallocate(S82)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S83(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X79),size(X79),'4123',1.000,
     & X79,S83)
       deallocate(S83)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S84(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S84)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S84),size(S84),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,M1-M1/),'2341',S84,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z154(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z154)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z154)
       deallocate(Z154)
       deallocate(S84)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S85(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S85)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S85),size(S85),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N2-N2,M1-M1/),'2341',S85,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N2,N2-N2,N0-N0,N0-N0/),'621345',t3D,F2)
       allocate(Z155(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z155)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z155)
       deallocate(Z155)
       deallocate(S85)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N0-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S86(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S86)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S86),size(S86),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S86,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(Z156(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K0*K0*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z156)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'123645',
     & 0.500,V3D,Z156)
       call sum_stripe(6,shape(V3D),size(V3D),'123654',
     & -0.500,V3D,Z156)
       deallocate(Z156)
       deallocate(S86)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N0-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S87(N0+1:M1,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S87)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S87),size(S87),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N0-N0,N0-N0/),'2314',S87,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(Z157(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K0*K0*K6
       I3=K8*K5
       call EGEMM(I1,I2,I3,D1,F2,Z157)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'123645',
     & 1.000,V3D,Z157)
       call sum_stripe(6,shape(V3D),size(V3D),'123654',
     & -1.000,V3D,Z157)
       deallocate(Z157)
       deallocate(S87)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,N0-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S88(N0+1:M1,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S88)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S88),size(S88),shape(D1),size(D1),
     & (/M1-M1,M1-M1,N0-N0,N0-N0/),'2314',S88,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(Z158(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K0*K0*K6
       I3=K8*K8
       call EGEMM(I1,I2,I3,D1,F2,Z158)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'123645',
     & 0.500,V3D,Z158)
       call sum_stripe(6,shape(V3D),size(V3D),'123654',
     & -0.500,V3D,Z158)
       deallocate(Z158)
       deallocate(S88)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S89(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S89)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X37(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X37=0.0d0
       call sum_stripe(4,shape(X37),size(X37),'3124',-1.000,
     & X37,S89)
       deallocate(S89)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S90(M2+1:N3,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X38(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       X38=0.0d0
       call sum_stripe(4,shape(X38),size(X38),'3124',-1.000,
     & X38,S90)
       deallocate(S90)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S91(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X39(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       X39=0.0d0
       call sum_stripe(4,shape(X39),size(X39),'3124',-1.000,
     & X39,S91)
       deallocate(S91)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S92(M2+1:N3,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S92)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X40(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       X40=0.0d0
       call sum_stripe(4,shape(X40),size(X40),'3124',-1.000,
     & X40,S92)
       deallocate(S92)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S93(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X82(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X82=0.0d0
       call sum_stripe(4,shape(X82),size(X82),'3124',1.000,
     & X82,S93)
       deallocate(S93)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S94(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S94)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X83(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       X83=0.0d0
       call sum_stripe(4,shape(X83),size(X83),'3124',1.000,
     & X83,S94)
       deallocate(S94)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S95(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X84(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X84=0.0d0
       call sum_stripe(4,shape(X84),size(X84),'3124',1.000,
     & X84,S95)
       deallocate(S95)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S96(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S96)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X85(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       X85=0.0d0
       call sum_stripe(4,shape(X85),size(X85),'3124',1.000,
     & X85,S96)
       deallocate(S96)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N0-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S97(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X86(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X86=0.0d0
       call sum_stripe(4,shape(X86),size(X86),'3124',1.000,
     & X86,S97)
       deallocate(S97)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N0-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S98(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S98)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X87(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       X87=0.0d0
       call sum_stripe(4,shape(X87),size(X87),'3124',1.000,
     & X87,S98)
       deallocate(S98)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S99(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X88(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X88=0.0d0
       call sum_stripe(4,shape(X88),size(X88),'3124',1.000,
     & X88,S99)
       deallocate(S99)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N0-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S100(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S100)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X89(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       X89=0.0d0
       call sum_stripe(4,shape(X89),size(X89),'3124',1.000,
     & X89,S100)
       deallocate(S100)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q20(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q20)
       deallocate(D1)
       deallocate(B2)
C
       X22=X22+Q20
       deallocate(Q20)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N0-N0/),'3142',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q21(M1+1:N2,N0+1:M1))
       I1=K5*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q21)
       deallocate(D1)
       deallocate(B2)
C
       X23=X23+Q21
       deallocate(Q21)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S101(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X37),size(X37),'4123',-1.000,
     & X37,S101)
       deallocate(S101)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S102(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S102)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X38),size(X38),'4123',-1.000,
     & X38,S102)
       deallocate(S102)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S103(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X39),size(X39),'4123',-1.000,
     & X39,S103)
       deallocate(S103)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N2-N2,M2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S104(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S104)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X40),size(X40),'4123',-1.000,
     & X40,S104)
       deallocate(S104)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S105(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X82),size(X82),'4123',1.000,
     & X82,S105)
       deallocate(S105)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S106(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S106)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X83),size(X83),'4123',1.000,
     & X83,S106)
       deallocate(S106)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S107(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S107)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X84),size(X84),'4123',1.000,
     & X84,S107)
       deallocate(S107)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S108(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S108)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X85),size(X85),'4123',1.000,
     & X85,S108)
       deallocate(S108)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S109(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S109)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S109),size(S109),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,N0-N0/),'2341',S109,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z181(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z181)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z181)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z181)
       deallocate(Z181)
       deallocate(S109)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S110(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S110)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S110),size(S110),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,N0-N0/),'2341',S110,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z182(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z182)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z182)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z182)
       deallocate(Z182)
       deallocate(S110)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S111(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S111)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S111),size(S111),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2341',S111,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'421356',t3D,F2)
       allocate(Z183(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z183)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z183)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z183)
       deallocate(Z183)
       deallocate(S111)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S112(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S112)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S112),size(S112),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N2-N2,N0-N0/),'2341',S112,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'521346',t3D,F2)
       allocate(Z184(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z184)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z184)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z184)
       deallocate(Z184)
       deallocate(S112)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S113(N2+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       I1=K6*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S113)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X90(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       X90=0.0d0
       call sum_stripe(4,shape(X90),size(X90),'4123',1.000,
     & X90,S113)
       deallocate(S113)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S114(N2+1:M2,M2+1:N3,N2+1:M2,M2+1:N3))
       I1=K6*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S114)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X91(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       X91=0.0d0
       call sum_stripe(4,shape(X91),size(X91),'4123',1.000,
     & X91,S114)
       deallocate(S114)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S115(N2+1:M2,N2+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S115)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X92(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       X92=0.0d0
       call sum_stripe(4,shape(X92),size(X92),'4123',1.000,
     & X92,S115)
       deallocate(S115)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S116(N2+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       I1=K6*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S116)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X93(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       X93=0.0d0
       call sum_stripe(4,shape(X93),size(X93),'4123',1.000,
     & X93,S116)
       deallocate(S116)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S117(N2+1:M2,M2+1:N3,N2+1:M2,M2+1:N3))
       I1=K6*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S117)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X94(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       X94=0.0d0
       call sum_stripe(4,shape(X94),size(X94),'4123',1.000,
     & X94,S117)
       deallocate(S117)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S118(N2+1:M2,N2+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S118)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X95(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       X95=0.0d0
       call sum_stripe(4,shape(X95),size(X95),'4123',1.000,
     & X95,S118)
       deallocate(S118)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q22(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q22)
       deallocate(D1)
       deallocate(B2)
C
       X24=X24-Q22
       deallocate(Q22)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q23(N2+1:M2,M2+1:N3))
       I1=K6*K0
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q23)
       deallocate(D1)
       deallocate(B2)
C
       X25=X25-Q23
       deallocate(Q23)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S119(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S119)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X90),size(X90),'3124',-1.000,
     & X90,S119)
       deallocate(S119)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S120(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S120)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X91),size(X91),'3124',-1.000,
     & X91,S120)
       deallocate(S120)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S121(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S121)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X92),size(X92),'3124',-1.000,
     & X92,S121)
       deallocate(S121)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S122(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S122)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X93),size(X93),'3124',-1.000,
     & X93,S122)
       deallocate(S122)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S123(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S123)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X94),size(X94),'3124',-1.000,
     & X94,S123)
       deallocate(S123)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S124(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S124)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X95),size(X95),'3124',-1.000,
     & X95,S124)
       deallocate(S124)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S125(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S125)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X44(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       X44=0.0d0
       call sum_stripe(4,shape(X44),size(X44),'4123',-1.000,
     & X44,S125)
       deallocate(S125)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S126(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S126)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X45(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       X45=0.0d0
       call sum_stripe(4,shape(X45),size(X45),'4123',-1.000,
     & X45,S126)
       deallocate(S126)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q24(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q24)
       deallocate(D1)
       deallocate(B2)
C
       X26=X26-Q24
       deallocate(Q24)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q25(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q25)
       deallocate(D1)
       deallocate(B2)
C
       X27=X27-Q25
       deallocate(Q25)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S127(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S127)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X44),size(X44),'3124',1.000,
     & X44,S127)
       deallocate(S127)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S128(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S128)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X45),size(X45),'3124',1.000,
     & X45,S128)
       deallocate(S128)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q26(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q26)
       deallocate(D1)
       deallocate(B2)
C
       X28=X28-Q26
       deallocate(Q26)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q27(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q27)
       deallocate(D1)
       deallocate(B2)
C
       X29=X29-Q27
       deallocate(Q27)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S129(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S129)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S129)
       deallocate(S129)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S130(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S130)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X55),size(X55),'2314',-1.000,
     & X55,S130)
       deallocate(S130)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S131(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S131)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X56),size(X56),'2314',1.000,
     & X56,S131)
       deallocate(S131)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S132(M2+1:N3,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S132)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2413',-1.000,
     & X1,S132)
       deallocate(S132)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S133(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S133)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X55),size(X55),'2413',1.000,
     & X55,S133)
       deallocate(S133)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S134(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S134)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X56),size(X56),'2413',-1.000,
     & X56,S134)
       deallocate(S134)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,M2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S135(N2+1:M2,M1+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S135)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X57),size(X57),'3412',-1.000,
     & X57,S135)
       deallocate(S135)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,M2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S136(N2+1:M2,M1+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S136)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X58),size(X58),'3412',-1.000,
     & X58,S136)
       deallocate(S136)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S137(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S137)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X57),size(X57),'2413',1.000,
     & X57,S137)
       deallocate(S137)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S138(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S138)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X58),size(X58),'2413',1.000,
     & X58,S138)
       deallocate(S138)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S139(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S139)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'3412',1.000,X4,
     & S139)
       deallocate(S139)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S140(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S140)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2413',-1.000,
     & X4,S140)
       deallocate(S140)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S141(M2+1:N3,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S141)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X61),size(X61),'2314',1.000,
     & X61,S141)
       deallocate(S141)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S142(N2+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S142)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X62),size(X62),'2314',1.000,
     & X62,S142)
       deallocate(S142)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S143(N2+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S143)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X63),size(X63),'2314',1.000,
     & X63,S143)
       deallocate(S143)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,M2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S144(N2+1:M2,N0+1:M1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S144)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X64),size(X64),'3412',-1.000,
     & X64,S144)
       deallocate(S144)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,M2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S145(N2+1:M2,N0+1:M1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S145)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X65),size(X65),'3412',-1.000,
     & X65,S145)
       deallocate(S145)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S146(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S146)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X64),size(X64),'2413',1.000,
     & X64,S146)
       deallocate(S146)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S147(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S147)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X65),size(X65),'2413',1.000,
     & X65,S147)
       deallocate(S147)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S148(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S148)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'3412',1.000,X8,
     & S148)
       deallocate(S148)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S149(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S149)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2413',-1.000,
     & X8,S149)
       deallocate(S149)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S150(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S150)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S150),size(S150),shape(D1),size(D1),
     & (/M1-M1,M2-M2,M2-M2,M1-M1/),'3412',S150,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z228(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K5*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z228)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z228)
       deallocate(Z228)
       deallocate(S150)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S151(M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S151)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S151),size(S151),shape(D1),size(D1),
     & (/M1-M1,N1-N1,M2-M2,M1-M1/),'3412',S151,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z229(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K5*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z229)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z229)
       deallocate(Z229)
       deallocate(S151)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S152(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S152)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S152),size(S152),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,M1-M1/),'3412',S152,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z230(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z230)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z230)
       deallocate(Z230)
       deallocate(S152)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S153(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S153)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S153),size(S153),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N2-N2,M1-M1/),'3412',S153,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N1-N1,M2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z231(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z231)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z231)
       deallocate(Z231)
       deallocate(S153)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S154(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S154)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S154),size(S154),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,M1-M1/),'3412',S154,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z232(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z232)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z232)
       deallocate(Z232)
       deallocate(S154)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S155(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S155)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S155),size(S155),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N2-N2,M1-M1/),'3412',S155,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N1-N1,M2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z233(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z233)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z233)
       deallocate(Z233)
       deallocate(S155)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S156(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S156)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S156),size(S156),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N0-N0/),'3412',S156,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z234(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z234)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z234)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z234)
       deallocate(Z234)
       deallocate(S156)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S157(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S157)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S157),size(S157),shape(D1),size(D1),
     & (/M1-M1,M2-M2,M2-M2,N0-N0/),'3412',S157,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z235(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z235)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z235)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z235)
       deallocate(Z235)
       deallocate(S157)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S158(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S158)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S158),size(S158),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,N0-N0/),'3412',S158,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z236(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z236)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z236)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z236)
       deallocate(Z236)
       deallocate(S158)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S159(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S159)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S159),size(S159),shape(D1),size(D1),
     & (/M1-M1,N1-N1,M2-M2,N0-N0/),'3412',S159,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z237(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z237)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z237)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z237)
       deallocate(Z237)
       deallocate(S159)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S160(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S160)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S160),size(S160),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,N0-N0/),'3412',S160,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z238(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z238)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z238)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z238)
       deallocate(Z238)
       deallocate(S160)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S161(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S161)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S161),size(S161),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,N0-N0/),'3412',S161,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z239(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z239)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z239)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z239)
       deallocate(Z239)
       deallocate(S161)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S162(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S162)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S162),size(S162),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3412',S162,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z240(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z240)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z240)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z240)
       deallocate(Z240)
       deallocate(S162)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S163(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S163)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S163),size(S163),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N2-N2,N0-N0/),'3412',S163,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z241(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z241)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z241)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z241)
       deallocate(Z241)
       deallocate(S163)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S164(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S164)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S164),size(S164),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,N0-N0/),'3412',S164,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z242(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z242)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z242)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z242)
       deallocate(Z242)
       deallocate(S164)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S165(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S165)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S165),size(S165),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,N0-N0/),'3412',S165,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z243(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z243)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z243)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z243)
       deallocate(Z243)
       deallocate(S165)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S166(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S166)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S166),size(S166),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3412',S166,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z244(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z244)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z244)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z244)
       deallocate(Z244)
       deallocate(S166)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S167(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S167)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S167),size(S167),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N2-N2,N0-N0/),'3412',S167,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z245(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z245)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z245)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z245)
       deallocate(Z245)
       deallocate(S167)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S168(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S168)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S168),size(S168),shape(D1),size(D1),
     & (/M1-M1,M2-M2,M2-M2,M1-M1/),'3412',S168,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z246(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K5*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z246)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z246)
       deallocate(Z246)
       deallocate(S168)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S169(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S169)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S169),size(S169),shape(D1),size(D1),
     & (/M1-M1,N2-N2,M2-M2,M1-M1/),'3412',S169,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z247(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K5*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z247)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z247)
       deallocate(Z247)
       deallocate(S169)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S170(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S170)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S170),size(S170),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,M1-M1/),'3412',S170,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z248(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z248)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z248)
       deallocate(Z248)
       deallocate(S170)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S171(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S171)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S171),size(S171),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N2-N2,M1-M1/),'3412',S171,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N2,N2-N2,N0-N0,N0-N0/),'621345',t3D,F2)
       allocate(Z249(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z249)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & 1.000,V3D,Z249)
       deallocate(Z249)
       deallocate(S171)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S172(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S172)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X96(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X96=0.0d0
       call sum_stripe(4,shape(X96),size(X96),'3412',1.000,
     & X96,S172)
       deallocate(S172)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S173(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S173)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X97(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X97=0.0d0
       call sum_stripe(4,shape(X97),size(X97),'3412',1.000,
     & X97,S173)
       deallocate(S173)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',t2B,D2)
       allocate(Q28(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q28)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X46),size(X46),'21',1.000,
     & X46,Q28)
       deallocate(Q28)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S174(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S174)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S174),size(S174),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N0-N0/),'3412',S174,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z253(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z253)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z253)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z253)
       deallocate(Z253)
       deallocate(S174)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S175(M2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S175)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S175),size(S175),shape(D1),size(D1),
     & (/M1-M1,M2-M2,M2-M2,N0-N0/),'3412',S175,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z254(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z254)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & 1.000,V3D,Z254)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & -1.000,V3D,Z254)
       deallocate(Z254)
       deallocate(S175)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S176(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S176)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S176),size(S176),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,N0-N0/),'3412',S176,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z255(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z255)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z255)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z255)
       deallocate(Z255)
       deallocate(S176)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S177(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S177)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S177),size(S177),shape(D1),size(D1),
     & (/M1-M1,N2-N2,M2-M2,N0-N0/),'3412',S177,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z256(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z256)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & 1.000,V3D,Z256)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & -1.000,V3D,Z256)
       deallocate(Z256)
       deallocate(S177)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S178(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S178)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S178),size(S178),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,N0-N0/),'3412',S178,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z257(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z257)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z257)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z257)
       deallocate(Z257)
       deallocate(S178)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S179(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S179)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S179),size(S179),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,N0-N0/),'3412',S179,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z258(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z258)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z258)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z258)
       deallocate(Z258)
       deallocate(S179)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S180(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S180)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S180),size(S180),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S180,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'421356',t3D,F2)
       allocate(Z259(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z259)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z259)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z259)
       deallocate(Z259)
       deallocate(S180)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S181(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S181)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S181),size(S181),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N2-N2,N0-N0/),'3412',S181,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'521346',t3D,F2)
       allocate(Z260(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z260)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z260)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z260)
       deallocate(Z260)
       deallocate(S181)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S182(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S182)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X98(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X98=0.0d0
       call sum_stripe(4,shape(X98),size(X98),'3412',1.000,
     & X98,S182)
       deallocate(S182)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S183(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S183)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X99(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       X99=0.0d0
       call sum_stripe(4,shape(X99),size(X99),'3412',1.000,
     & X99,S183)
       deallocate(S183)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S184(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S184)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X100(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X100=0.0d0
       call sum_stripe(4,shape(X100),size(X100),'3412',
     & 1.000,X100,S184)
       deallocate(S184)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S185(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S185)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X101(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       X101=0.0d0
       call sum_stripe(4,shape(X101),size(X101),'3412',
     & 1.000,X101,S185)
       deallocate(S185)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(Q29(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q29)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X47),size(X47),'21',1.000,
     & X47,Q29)
       deallocate(Q29)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(Q30(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q30)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X48),size(X48),'21',1.000,
     & X48,Q30)
       deallocate(Q30)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',t2B,D2)
       allocate(Q31(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q31)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X49),size(X49),'21',-1.000,
     & X49,Q31)
       deallocate(Q31)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',t2B,D2)
       allocate(Q32(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q32)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X50),size(X50),'21',-1.000,
     & X50,Q32)
       deallocate(Q32)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',t2B,D2)
       allocate(Q33(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q33)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X51),size(X51),'21',-1.000,
     & X51,Q33)
       deallocate(Q33)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',t2B,D2)
       allocate(Q34(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q34)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X52),size(X52),'21',-1.000,
     & X52,Q34)
       deallocate(Q34)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',t2B,D2)
       allocate(Q35(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q35)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X53),size(X53),'21',-1.000,
     & X53,Q35)
       deallocate(Q35)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',t2B,D2)
       allocate(Q36(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q36)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X54),size(X54),'21',-1.000,
     & X54,Q36)
       deallocate(Q36)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,N2-N2/),'21',FBHP,B1)
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S186(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,D2,S186)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S186)
       deallocate(S186)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S187(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S187)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X55),size(X55),'2341',1.000,
     & X55,S187)
       deallocate(S187)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S188(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S188)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X56),size(X56),'2341',-1.000,
     & X56,S188)
       deallocate(S188)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S189(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S189)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X60),size(X60),'2341',-1.000,
     & X60,S189)
       deallocate(S189)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S190(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S190)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X59),size(X59),'2341',1.000,
     & X59,S190)
       deallocate(S190)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,N2-N2/),'21',FBHP,B1)
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S191(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,D2,S191)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S191)
       deallocate(S191)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S192(N2+1:M2,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S192)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2314',0.500,X4,
     & S192)
       deallocate(S192)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'3412',VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S193(M2+1:N3,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S193)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X102(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X102=0.0d0
       call sum_stripe(4,shape(X102),size(X102),'2314',
     & 1.000,X102,S193)
       deallocate(S193)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'4132',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S194(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S194)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X103(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       X103=0.0d0
       call sum_stripe(4,shape(X103),size(X103),'2314',
     & 1.000,X103,S194)
       deallocate(S194)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S195(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S195)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S195)
       deallocate(S195)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S196(N2+1:M2,N2+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S196)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2314',0.500,X8,
     & S196)
       deallocate(S196)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S197(M2+1:N3,N2+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S197)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X104(N2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       X104=0.0d0
       call sum_stripe(4,shape(X104),size(X104),'2314',
     & 1.000,X104,S197)
       deallocate(S197)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'4132',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S198(N2+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S198)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S198),size(S198),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'3124',S198,D1)
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z285(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,D2,Z285)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & 1.000,V3D,Z285)
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & -1.000,V3D,Z285)
       call sum_stripe(6,shape(V3D),size(V3D),'126354',
     & -1.000,V3D,Z285)
       call sum_stripe(6,shape(V3D),size(V3D),'136254',
     & 1.000,V3D,Z285)
       deallocate(Z285)
       deallocate(S198)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S199(M2+1:N3,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S199)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2413',-1.000,
     & X1,S199)
       deallocate(S199)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S200(M2+1:N3,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S200)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X61),size(X61),'2314',1.000,
     & X61,S200)
       deallocate(S200)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S201(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S201)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X2(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       X2=0.0d0
       call sum_stripe(4,shape(X2),size(X2),'2413',-1.000,
     & X2,S201)
       deallocate(S201)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M2-N2/),'4213',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S202(N2+1:M2,N0+1:M1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S202)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X104),size(X104),'3412',
     & 2.000,X104,S202)
       deallocate(S202)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,M2-N2/),'1243',VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S203(N0+1:M1,M1+1:N2,N0+1:N2,M2+1:N3))
       I1=K6*K2
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S203)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'3412',0.500,X1,
     & S203)
       deallocate(S203)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4213',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S204(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S204)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S204),size(S204),shape(D1),size(D1),
     & (/N2-N2,N2-N2,N2-N2,N0-N0/),'3412',S204,D1)
       allocate(D2(N2+1:N3,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,M2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z291(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K0
       I2=K8*K5*K6
       I3=K4
       call EGEMM(I1,I2,I3,D1,D2,Z291)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'156234',
     & -1.000,V3D,Z291)
       call sum_stripe(6,shape(V3D),size(V3D),'156324',
     & 1.000,V3D,Z291)
       call sum_stripe(6,shape(V3D),size(V3D),'146235',
     & 1.000,V3D,Z291)
       call sum_stripe(6,shape(V3D),size(V3D),'146325',
     & -1.000,V3D,Z291)
       deallocate(Z291)
       deallocate(S204)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S205(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S205)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X7(N2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       X7=0.0d0
       call sum_stripe(4,shape(X7),size(X7),'2413',1.000,X7,
     & S205)
       deallocate(S205)
C
       call sumx_sorted2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M2,N3,N2,M2,N0,M1,X7,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z7(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K6
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X7,D2,Z7)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'346125',
     & 1.000,V3D,Z7)
       call sum_stripe(6,shape(V3D),size(V3D),'246135',
     & -1.000,V3D,Z7)
       call sum_stripe(6,shape(V3D),size(V3D),'356124',
     & -1.000,V3D,Z7)
       call sum_stripe(6,shape(V3D),size(V3D),'256134',
     & 1.000,V3D,Z7)
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N2-N2/),'1243',VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S206(N0+1:M1,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S206)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'3412',0.500,X2,
     & S206)
       deallocate(S206)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,M2-N2/),'1243',VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S207(N0+1:M1,N0+1:M1,N0+1:N2,M2+1:N3))
       I1=K6*K2
       I2=K5*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S207)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'3412',0.500,X5,
     & S207)
       deallocate(S207)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M2-N2/),'4123',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S208(N2+1:M2,M1+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S208)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X3(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X3=0.0d0
       call sum_stripe(4,shape(X3),size(X3),'3412',-1.000,
     & X3,S208)
       deallocate(S208)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N2-N2/),'1243',VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S209(N0+1:M1,N0+1:M1,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K5*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S209)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X6(N0+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       X6=0.0d0
       call sum_stripe(4,shape(X6),size(X6),'3412',0.500,X6,
     & S209)
       deallocate(S209)
C
       call sumx_sorted2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,M2,N0,M1,N0,M1,X6,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z6(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X6,D2,Z6)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & 1.000,V3D,Z6)
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & -1.000,V3D,Z6)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S210(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S210)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2413',1.000,X3,
     & S210)
       deallocate(S210)
C
       call sumx_sorted2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M2,N3,N2,M2,M1,N2,X3,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(Z3(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X3,D2,Z3)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345126',
     & -1.000,V3D,Z3)
       call sum_stripe(6,shape(V3D),size(V3D),'245136',
     & 1.000,V3D,Z3)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4213',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S211(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S211)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S211),size(S211),shape(D1),size(D1),
     & (/N2-N2,N2-N2,N2-N2,M1-M1/),'3412',S211,D1)
       allocate(D2(N2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,M2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(Z298(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K5*K5*K6
       I3=K4
       call EGEMM(I1,I2,I3,D1,D2,Z298)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'145236',
     & -1.000,V3D,Z298)
       call sum_stripe(6,shape(V3D),size(V3D),'145326',
     & 1.000,V3D,Z298)
       deallocate(Z298)
       deallocate(S211)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-N1,N2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S212(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S212)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S212)
       deallocate(S212)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,N2-N2,N2-N2,N0-N0/),'463125',t3C4,F2)
       allocate(S213(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S213)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S213)
       deallocate(S213)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,M2-N1,N2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S214(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S214)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S214)
       deallocate(S214)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N1-N1,N2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S215(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S215)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S215)
       deallocate(S215)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,N2-N2,N2-N2,N0-N0/),'463125',t3C4,F2)
       allocate(S216(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S216)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S216)
       deallocate(S216)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,N1-N1,N2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S217(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S217)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S217)
       deallocate(S217)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-N1,M2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S218(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S218)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X104),size(X104),'2341',
     & 2.000,X104,S218)
       deallocate(S218)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,M2-N2,N2-N2,N0-N0/),'463125',t3C4,F2)
       allocate(S219(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S219)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X104),size(X104),'2341',
     & -2.000,X104,S219)
       deallocate(S219)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,M2-N1,M2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S220(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S220)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X104),size(X104),'2341',
     & 2.000,X104,S220)
       deallocate(S220)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N1-N1,M2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S221(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S221)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X104),size(X104),'2341',
     & 2.000,X104,S221)
       deallocate(S221)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,M2-N2,N2-N2,N0-N0/),'463125',t3C4,F2)
       allocate(S222(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S222)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X104),size(X104),'2341',
     & -2.000,X104,S222)
       deallocate(S222)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,N1-N1,M2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S223(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S223)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X104),size(X104),'2341',
     & 2.000,X104,S223)
       deallocate(S223)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N2-N2,N0-N0,N0-N0/),'613245',t3C4,F2)
       allocate(S224(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S224)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X105(N0+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       X105=0.0d0
       call sum_stripe(4,shape(X105),size(X105),'2341',
     & 1.000,X105,S224)
       deallocate(S224)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N2-N2,N0-N0,N0-N0/),'613245',t3C4,F2)
       allocate(S225(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S225)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X105),size(X105),'2341',
     & 1.000,X105,S225)
       deallocate(S225)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N2-N2,N0-N0,N0-N0/),'613245',t3C4,F2)
       allocate(S226(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S226)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X105),size(X105),'2341',
     & 1.000,X105,S226)
       deallocate(S226)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N2-N2,N0-N0,N0-N0/),'613245',t3C4,F2)
       allocate(S227(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S227)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X105),size(X105),'2341',
     & 1.000,X105,S227)
       deallocate(S227)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S228(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S228)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'3412',1.000,X9,
     & S228)
       deallocate(S228)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S229(M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S229)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'3412',1.000,
     & X10,S229)
       deallocate(S229)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,M2-N2,N0-N0,N0-N0/),'623145',t3C4,F2)
       allocate(S230(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S230)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',1.000,X5,
     & S230)
       deallocate(S230)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C2),size(t3C2),shape(F2),size(F2),
     & (/M1-M1,M2-M2,N1-N1,M2-M2,N0-N0,N0-N0/),'613245',t3C2,F2)
       allocate(S231(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S231)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S231)
       deallocate(S231)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,M2-N2,N0-N0,N0-N0/),'623145',t3C4,F2)
       allocate(S232(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K6
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S232)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',1.000,X5,
     & S232)
       deallocate(S232)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S233(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S233)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X11(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X11=0.0d0
       call sum_stripe(4,shape(X11),size(X11),'3412',1.000,
     & X11,S233)
       deallocate(S233)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,M1,N2,X11,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z11(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X11,F2,Z11)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z11)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z11)
       deallocate(Z11)
       deallocate(X11)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S234(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S234)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X12(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X12=0.0d0
       call sum_stripe(4,shape(X12),size(X12),'3412',1.000,
     & X12,S234)
       deallocate(S234)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,M1,N2,X12,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N1-N1,M2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z12(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X12,F2,Z12)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z12)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z12)
       deallocate(Z12)
       deallocate(X12)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S235(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S235)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S235)
       deallocate(S235)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S236(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S236)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S236)
       deallocate(S236)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S237(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S237)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S237)
       deallocate(S237)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S238(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S238)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S238)
       deallocate(S238)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S239(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S239)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S239)
       deallocate(S239)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N1-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S240(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S240)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S240)
       deallocate(S240)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S241(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S241)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S241)
       deallocate(S241)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,N2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S242(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S242)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S242)
       deallocate(S242)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S243(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S243)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X102),size(X102),'2341',
     & -2.000,X102,S243)
       deallocate(S243)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S244(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S244)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X102),size(X102),'2341',
     & -2.000,X102,S244)
       deallocate(S244)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S245(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S245)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X102),size(X102),'2341',
     & -2.000,X102,S245)
       deallocate(S245)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S246(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S246)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X102),size(X102),'2341',
     & -2.000,X102,S246)
       deallocate(S246)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S247(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S247)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X102),size(X102),'2341',
     & -2.000,X102,S247)
       deallocate(S247)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N1-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S248(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S248)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X102),size(X102),'2341',
     & -2.000,X102,S248)
       deallocate(S248)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S249(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S249)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X102),size(X102),'2341',
     & -2.000,X102,S249)
       deallocate(S249)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,M2-N2,N2-N2,M1-M1/),'463125',t3C1,F2)
       allocate(S250(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S250)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X102),size(X102),'2341',
     & -2.000,X102,S250)
       deallocate(S250)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S251(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S251)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X103),size(X103),'2341',
     & 1.000,X103,S251)
       deallocate(S251)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S252(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S252)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X103),size(X103),'2341',
     & 1.000,X103,S252)
       deallocate(S252)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S253(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S253)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X103),size(X103),'2341',
     & 1.000,X103,S253)
       deallocate(S253)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S254(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S254)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X103),size(X103),'2341',
     & 1.000,X103,S254)
       deallocate(S254)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S255(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S255)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X103),size(X103),'2341',
     & 1.000,X103,S255)
       deallocate(S255)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S256(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S256)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X103),size(X103),'2341',
     & 1.000,X103,S256)
       deallocate(S256)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S257(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S257)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X103),size(X103),'2341',
     & 1.000,X103,S257)
       deallocate(S257)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,N2-N2,N0-N0,M1-M1/),'613245',t3C1,F2)
       allocate(S258(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S258)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X103),size(X103),'2341',
     & 1.000,X103,S258)
       deallocate(S258)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(Z281(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K5*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X103,D2,Z281)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'125346',
     & -1.000,V3D,Z281)
       call sum_stripe(6,shape(V3D),size(V3D),'135246',
     & 1.000,V3D,Z281)
       call sum_stripe(6,shape(V3D),size(V3D),'134256',
     & -1.000,V3D,Z281)
       call sum_stripe(6,shape(V3D),size(V3D),'124356',
     & 1.000,V3D,Z281)
       deallocate(Z281)
       deallocate(X103)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S259(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S259)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'3412',1.000,
     & X13,S259)
       deallocate(S259)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S260(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S260)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'3412',1.000,
     & X14,S260)
       deallocate(S260)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S261(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S261)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X15),size(X15),'3412',1.000,
     & X15,S261)
       deallocate(S261)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S262(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S262)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X16),size(X16),'3412',1.000,
     & X16,S262)
       deallocate(S262)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,M2-N2,N0-N0,M1-M1/),'623145',t3C1,F2)
       allocate(S263(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S263)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S263)
       deallocate(S263)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,M2-N2,N0-N0,M1-M1/),'623145',t3C1,F2)
       allocate(S264(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S264)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S264)
       deallocate(S264)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,N1-N1,M2-M2,N0-N0,M1-M1/),'613245',t3C3,F2)
       allocate(S265(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S265)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S265)
       deallocate(S265)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,N1-N1,M2-M2,N0-N0,M1-M1/),'613245',t3C3,F2)
       allocate(S266(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S266)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S266)
       deallocate(S266)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,M2-N2,N0-N0,M1-M1/),'623145',t3C1,F2)
       allocate(S267(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S267)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S267)
       deallocate(S267)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,M2-N2,N0-N0,M1-M1/),'623145',t3C1,F2)
       allocate(S268(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S268)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S268)
       deallocate(S268)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S269(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S269)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X17(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X17=0.0d0
       call sum_stripe(4,shape(X17),size(X17),'3412',1.000,
     & X17,S269)
       deallocate(S269)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,N0,M1,X17,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z17(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X17,F2,Z17)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z17)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z17)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z17)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z17)
       deallocate(Z17)
       deallocate(X17)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S270(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S270)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X18(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X18=0.0d0
       call sum_stripe(4,shape(X18),size(X18),'3412',1.000,
     & X18,S270)
       deallocate(S270)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,N0,M1,X18,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z18(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X18,F2,Z18)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z18)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z18)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z18)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z18)
       deallocate(Z18)
       deallocate(X18)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S271(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S271)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X19(N0+1:M1,N1+1:M2,N2+1:M2,N0+1:M1))
       X19=0.0d0
       call sum_stripe(4,shape(X19),size(X19),'3412',1.000,
     & X19,S271)
       deallocate(S271)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,N2,M2,N0,M1,X19,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z19(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X19,F2,Z19)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z19)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z19)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z19)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z19)
       deallocate(Z19)
       deallocate(X19)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S272(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S272)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X20(M1+1:N1,N1+1:M2,N2+1:M2,N0+1:M1))
       X20=0.0d0
       call sum_stripe(4,shape(X20),size(X20),'3412',1.000,
     & X20,S272)
       deallocate(S272)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,N0,M1,X20,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z20(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X20,F2,Z20)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z20)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z20)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z20)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z20)
       deallocate(Z20)
       deallocate(X20)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,N2-N2,N2-N2,N0-N0/),'461235',t3D,F2)
       allocate(S273(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S273)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S273)
       deallocate(S273)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,N2-N2,N2-N2,N0-N0/),'561234',t3D,F2)
       allocate(S274(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S274)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',0.500,X8,
     & S274)
       deallocate(S274)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,N2-N2,N2-N2,N0-N0/),'461235',t3D,F2)
       allocate(S275(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S275)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S275)
       deallocate(S275)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,N2-N2,N2-N2,N0-N0/),'561234',t3D,F2)
       allocate(S276(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S276)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',0.500,X8,
     & S276)
       deallocate(S276)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,M2-N2,N2-N2,N0-N0/),'461235',t3D,F2)
       allocate(S277(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S277)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',1.000,
     & X65,S277)
       deallocate(S277)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,M2-N2,N2-N2,N0-N0/),'561234',t3D,F2)
       allocate(S278(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S278)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',-0.500,
     & X65,S278)
       deallocate(S278)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,M2-N2,N2-N2,N0-N0/),'462135',t3D,F2)
       allocate(S279(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S279)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',-1.000,
     & X65,S279)
       deallocate(S279)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,M2-N2,N2-N2,N0-N0/),'562134',t3D,F2)
       allocate(S280(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S280)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X65),size(X65),'2341',0.500,
     & X65,S280)
       deallocate(S280)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,M2-N2,N2-N2,N0-N0/),'461235',t3D,F2)
       allocate(S281(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S281)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X64),size(X64),'2341',-1.000,
     & X64,S281)
       deallocate(S281)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,M2-N2,N2-N2,N0-N0/),'561234',t3D,F2)
       allocate(S282(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S282)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X64),size(X64),'2341',0.500,
     & X64,S282)
       deallocate(S282)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,M2-N2,N2-N2,N0-N0/),'462135',t3D,F2)
       allocate(S283(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S283)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X64),size(X64),'2341',1.000,
     & X64,S283)
       deallocate(S283)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,M2-N2,N2-N2,N0-N0/),'562134',t3D,F2)
       allocate(S284(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S284)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X64),size(X64),'2341',-0.500,
     & X64,S284)
       deallocate(S284)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S285(N0+1:M1,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S285)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X30),size(X30),'3412',0.500,
     & X30,S285)
       deallocate(S285)
C
       allocate(D1(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S286(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S286)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X31),size(X31),'3412',0.500,
     & X31,S286)
       deallocate(S286)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(S287(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S287)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X60),size(X60),'2341',0.500,
     & X60,S287)
       deallocate(S287)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(S288(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S288)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X60),size(X60),'2341',1.000,
     & X60,S288)
       deallocate(S288)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(S289(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S289)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X60),size(X60),'2341',0.500,
     & X60,S289)
       deallocate(S289)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(S290(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S290)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X59),size(X59),'2341',0.500,
     & X59,S290)
       deallocate(S290)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(S291(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S291)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X59),size(X59),'2341',1.000,
     & X59,S291)
       deallocate(S291)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(S292(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S292)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X59),size(X59),'2341',0.500,
     & X59,S292)
       deallocate(S292)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S293(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S293)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X32),size(X32),'3412',-1.000,
     & X32,S293)
       deallocate(S293)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S294(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S294)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X33),size(X33),'3412',-1.000,
     & X33,S294)
       deallocate(S294)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,M2-N2,N0-N0,N0-N0/),'613245',t3D,F2)
       allocate(S295(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S295)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',1.000,X5,
     & S295)
       deallocate(S295)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,M2-N2,N0-N0,N0-N0/),'623145',t3D,F2)
       allocate(S296(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S296)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-0.500,
     & X5,S296)
       deallocate(S296)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S297(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S297)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X78),size(X78),'3412',1.000,
     & X78,S297)
       deallocate(S297)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S298(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S298)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X79),size(X79),'3412',1.000,
     & X79,S298)
       deallocate(S298)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S299(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S299)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X96),size(X96),'3412',1.000,
     & X96,S299)
       deallocate(S299)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z250(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X96,F2,Z250)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z250)
       deallocate(Z250)
       deallocate(X96)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S300(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S300)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X97),size(X97),'3412',1.000,
     & X97,S300)
       deallocate(S300)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N2,N2-N2,N0-N0,N0-N0/),'621345',t3D,F2)
       allocate(Z251(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X97,F2,Z251)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z251)
       deallocate(Z251)
       deallocate(X97)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Q37(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q37)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X21),size(X21),'21',0.500,
     & X21,Q37)
       deallocate(Q37)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S301(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S301)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',0.500,X4,
     & S301)
       deallocate(S301)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S302(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S302)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S302)
       deallocate(S302)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S303(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S303)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',0.500,X4,
     & S303)
       deallocate(S303)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S304(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S304)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',0.500,X4,
     & S304)
       deallocate(S304)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S305(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S305)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S305)
       deallocate(S305)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S306(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S306)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',0.500,X4,
     & S306)
       deallocate(S306)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,M2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S307(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S307)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X58),size(X58),'2341',-0.500,
     & X58,S307)
       deallocate(S307)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,M2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S308(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S308)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X58),size(X58),'2341',-1.000,
     & X58,S308)
       deallocate(S308)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,M2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S309(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S309)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X58),size(X58),'2341',-0.500,
     & X58,S309)
       deallocate(S309)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,M2-N2,N2-N2,M1-M1/),'452136',t3D,F2)
       allocate(S310(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S310)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X58),size(X58),'2341',0.500,
     & X58,S310)
       deallocate(S310)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,M2-N2,N2-N2,M1-M1/),'452136',t3D,F2)
       allocate(S311(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S311)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X58),size(X58),'2341',1.000,
     & X58,S311)
       deallocate(S311)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,M2-N2,N2-N2,M1-M1/),'452136',t3D,F2)
       allocate(S312(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S312)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X58),size(X58),'2341',0.500,
     & X58,S312)
       deallocate(S312)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,M2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S313(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S313)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X57),size(X57),'2341',0.500,
     & X57,S313)
       deallocate(S313)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,M2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S314(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S314)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X57),size(X57),'2341',1.000,
     & X57,S314)
       deallocate(S314)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,M2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(S315(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S315)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X57),size(X57),'2341',0.500,
     & X57,S315)
       deallocate(S315)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,M2-N2,N2-N2,M1-M1/),'452136',t3D,F2)
       allocate(S316(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S316)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X57),size(X57),'2341',-0.500,
     & X57,S316)
       deallocate(S316)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,M2-N2,N2-N2,M1-M1/),'452136',t3D,F2)
       allocate(S317(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S317)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X57),size(X57),'2341',-1.000,
     & X57,S317)
       deallocate(S317)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,M2-N2,N2-N2,M1-M1/),'452136',t3D,F2)
       allocate(S318(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S318)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X57),size(X57),'2341',-0.500,
     & X57,S318)
       deallocate(S318)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S319(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K5*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S319)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X34(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       X34=0.0d0
       call sum_stripe(4,shape(X34),size(X34),'3412',0.500,
     & X34,S319)
       deallocate(S319)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S320(N0+1:M1,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K5*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S320)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X35(N0+1:M1,M1+1:N2,N0+1:M1,N0+1:M1))
       X35=0.0d0
       call sum_stripe(4,shape(X35),size(X35),'3412',0.500,
     & X35,S320)
       deallocate(S320)
C
       allocate(D1(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S321(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K5*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S321)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X36(M1+1:N2,M1+1:N2,N0+1:M1,N0+1:M1))
       X36=0.0d0
       call sum_stripe(4,shape(X36),size(X36),'3412',0.500,
     & X36,S321)
       deallocate(S321)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(S322(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S322)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X56),size(X56),'2341',0.500,
     & X56,S322)
       deallocate(S322)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(S323(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S323)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X56),size(X56),'2341',-0.500,
     & X56,S323)
       deallocate(S323)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(S324(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S324)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X56),size(X56),'2341',1.000,
     & X56,S324)
       deallocate(S324)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(S325(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S325)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X56),size(X56),'2341',-1.000,
     & X56,S325)
       deallocate(S325)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(S326(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S326)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X56),size(X56),'2341',0.500,
     & X56,S326)
       deallocate(S326)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(S327(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S327)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X56),size(X56),'2341',-0.500,
     & X56,S327)
       deallocate(S327)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(S328(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S328)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X55),size(X55),'2341',0.500,
     & X55,S328)
       deallocate(S328)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(S329(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S329)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X55),size(X55),'2341',-0.500,
     & X55,S329)
       deallocate(S329)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(S330(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S330)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X55),size(X55),'2341',1.000,
     & X55,S330)
       deallocate(S330)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(S331(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S331)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X55),size(X55),'2341',-1.000,
     & X55,S331)
       deallocate(S331)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(S332(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S332)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X55),size(X55),'2341',0.500,
     & X55,S332)
       deallocate(S332)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(S333(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S333)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X55),size(X55),'2341',-0.500,
     & X55,S333)
       deallocate(S333)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S334(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S334)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X37),size(X37),'3412',-1.000,
     & X37,S334)
       deallocate(S334)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S335(M2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S335)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X38),size(X38),'3412',-1.000,
     & X38,S335)
       deallocate(S335)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S336(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S336)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X39),size(X39),'3412',-1.000,
     & X39,S336)
       deallocate(S336)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S337(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S337)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X40),size(X40),'3412',-1.000,
     & X40,S337)
       deallocate(S337)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,M2-N2,N0-N0,M1-M1/),'413256',t3D,F2)
       allocate(S338(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S338)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S338)
       deallocate(S338)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,M2-N2,N0-N0,M1-M1/),'513246',t3D,F2)
       allocate(S339(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S339)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S339)
       deallocate(S339)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,M2-N2,N0-N0,M1-M1/),'423156',t3D,F2)
       allocate(S340(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S340)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-0.500,
     & X1,S340)
       deallocate(S340)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,M2-N2,N0-N0,M1-M1/),'523146',t3D,F2)
       allocate(S341(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S341)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',0.500,X1,
     & S341)
       deallocate(S341)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S342(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S342)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X82),size(X82),'3412',1.000,
     & X82,S342)
       deallocate(S342)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S343(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S343)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X83),size(X83),'3412',1.000,
     & X83,S343)
       deallocate(S343)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S344(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S344)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X84),size(X84),'3412',1.000,
     & X84,S344)
       deallocate(S344)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S345(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S345)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X85),size(X85),'3412',1.000,
     & X85,S345)
       deallocate(S345)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S346(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S346)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X98),size(X98),'3412',1.000,
     & X98,S346)
       deallocate(S346)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z261(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X98,F2,Z261)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z261)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z261)
       deallocate(Z261)
       deallocate(X98)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S347(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S347)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X99),size(X99),'3412',1.000,
     & X99,S347)
       deallocate(S347)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z262(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X99,F2,Z262)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z262)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z262)
       deallocate(Z262)
       deallocate(X99)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S348(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S348)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X100),size(X100),'3412',
     & 1.000,X100,S348)
       deallocate(S348)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'421356',t3D,F2)
       allocate(Z263(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X100,F2,Z263)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z263)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z263)
       deallocate(Z263)
       deallocate(X100)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S349(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S349)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X101),size(X101),'3412',
     & 1.000,X101,S349)
       deallocate(S349)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'521346',t3D,F2)
       allocate(Z264(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X101,F2,Z264)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z264)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z264)
       deallocate(Z264)
       deallocate(X101)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(Q38(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q38)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X22),size(X22),'21',0.500,
     & X22,Q38)
       deallocate(Q38)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(Q39(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q39)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X23),size(X23),'21',0.500,
     & X23,Q39)
       deallocate(Q39)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S350(M2+1:N3,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S350)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X41(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       X41=0.0d0
       call sum_stripe(4,shape(X41),size(X41),'3412',0.500,
     & X41,S350)
       deallocate(S350)
C
       call sumx_sorted1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,M2,N3,M2,N3,N2,M2,X41,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,M2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z47(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K5*K5*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,X41,F2,Z47)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345612',
     & 0.500,V3D,Z47)
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & -0.500,V3D,Z47)
       deallocate(Z47)
       deallocate(X41)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S351(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S351)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X42(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       X42=0.0d0
       call sum_stripe(4,shape(X42),size(X42),'3412',0.500,
     & X42,S351)
       deallocate(S351)
C
       call sumx_sorted1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,N2,M2,M2,N3,N2,M2,X42,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z48(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K5*K5*K0
       I3=K0*K6
       call EGEMM(I1,I2,I3,X42,F2,Z48)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345612',
     & 1.000,V3D,Z48)
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & -1.000,V3D,Z48)
       deallocate(Z48)
       deallocate(X42)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S352(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S352)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X43(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       X43=0.0d0
       call sum_stripe(4,shape(X43),size(X43),'3412',0.500,
     & X43,S352)
       deallocate(S352)
C
       call sumx_sorted1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,M2,N2,M2,M2,N3,N2,M2,X43,VCAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z49(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K5*K5*K0
       I3=K0*K0
       call EGEMM(I1,I2,I3,X43,F2,Z49)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345612',
     & 0.500,V3D,Z49)
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & -0.500,V3D,Z49)
       deallocate(Z49)
       deallocate(X43)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,M2-N2/),'3412',t2C,D2)
       allocate(Q40(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q40)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X24),size(X24),'21',-0.500,
     & X24,Q40)
       deallocate(Q40)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,M2-N2/),'3412',t2C,D2)
       allocate(Q41(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q41)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X25),size(X25),'21',-0.500,
     & X25,Q41)
       deallocate(Q41)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S353(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S353)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X44),size(X44),'3412',0.500,
     & X44,S353)
       deallocate(S353)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S354(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S354)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X45),size(X45),'3412',0.500,
     & X45,S354)
       deallocate(S354)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(Q42(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q42)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X26),size(X26),'21',-0.500,
     & X26,Q42)
       deallocate(Q42)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(Q43(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q43)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X27),size(X27),'21',-0.500,
     & X27,Q43)
       deallocate(Q43)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N2/),'3421',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(Q44(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q44)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X53),size(X53),'21',0.500,
     & X53,Q44)
       deallocate(Q44)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(Q45(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q45)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X54),size(X54),'21',0.500,
     & X54,Q45)
       deallocate(Q45)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q46(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(Q46),size(Q46),shape(B1),size(B1),
     & (/N2-N2,M1-M1/),'21',Q46,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q50(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q50)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X48),size(X48),'21',1.000,
     & X48,Q50)
       deallocate(Q50)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z54(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K5*K0*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X48,F2,Z54)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'123465',
     & -1.000,V3D,Z54)
       call sum_stripe(6,shape(V3D),size(V3D),'123564',
     & 1.000,V3D,Z54)
       deallocate(Z54)
       deallocate(X48)
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
       call sum_stripe(2,shape(X46),size(X46),'21',1.000,
     & X46,Q47)
       deallocate(Q47)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z52(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K5*K0*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X46,F2,Z52)
       deallocate(F2)
C
       V3D=V3D-Z52
       deallocate(Z52)
       deallocate(X46)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q48(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q48),size(Q48),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q48,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q49(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q49)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X47),size(X47),'21',1.000,
     & X47,Q49)
       deallocate(Q49)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z53(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K5*K0*K0*K6
       I3=K5
       call EGEMM(I1,I2,I3,X47,F2,Z53)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'123465',
     & 1.000,V3D,Z53)
       call sum_stripe(6,shape(V3D),size(V3D),'123564',
     & -1.000,V3D,Z53)
       deallocate(Z53)
       deallocate(X47)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q51(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q55(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q51,B2,Q55)
       deallocate(B2)
C
       call sum_stripe(2,shape(X51),size(X51),'21',-1.000,
     & X51,Q55)
       deallocate(Q55)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,M2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z57(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K5*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,X51,F2,Z57)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134562',
     & -1.000,V3D,Z57)
       deallocate(Z57)
       deallocate(X51)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q57(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q51,B2,Q57)
       deallocate(B2)
C
       call sum_stripe(2,shape(X53),size(X53),'21',-1.000,
     & X53,Q57)
       deallocate(Q57)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,M2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z59(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K5*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,X53,F2,Z59)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124563',
     & 1.000,V3D,Z59)
       deallocate(Z59)
       deallocate(X53)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q52(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q51,B2,Q52)
       deallocate(B2)
C
       call sum_stripe(2,shape(X49),size(X49),'21',-1.000,
     & X49,Q52)
       deallocate(Q52)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z55(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K5*K5*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,X49,F2,Z55)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234561',
     & 1.000,V3D,Z55)
       deallocate(Z55)
       deallocate(X49)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q53(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q54(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q53,B2,Q54)
       deallocate(B2)
C
       call sum_stripe(2,shape(X50),size(X50),'21',-1.000,
     & X50,Q54)
       deallocate(Q54)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z56(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K5*K5*K0*K0
       I3=K0
       call EGEMM(I1,I2,I3,X50,F2,Z56)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234561',
     & 1.000,V3D,Z56)
       deallocate(Z56)
       deallocate(X50)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q58(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q53,B2,Q58)
       deallocate(B2)
C
       call sum_stripe(2,shape(X54),size(X54),'21',-1.000,
     & X54,Q58)
       deallocate(Q58)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,M2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'213456',t3D,F2)
       allocate(Z60(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K5*K0*K6
       I3=K0
       call EGEMM(I1,I2,I3,X54,F2,Z60)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124563',
     & -1.000,V3D,Z60)
       deallocate(Z60)
       deallocate(X54)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q56(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q53,B2,Q56)
       deallocate(B2)
C
       call sum_stripe(2,shape(X52),size(X52),'21',-1.000,
     & X52,Q56)
       deallocate(Q56)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,M2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'213456',t3D,F2)
       allocate(Z58(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K5*K0*K6
       I3=K0
       call EGEMM(I1,I2,I3,X52,F2,Z58)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134562',
     & 1.000,V3D,Z58)
       deallocate(Z58)
       deallocate(X52)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q59(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q59),size(Q59),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q59,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S356(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S356)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S356)
       deallocate(S356)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q59),size(Q59),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q59,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S357(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S357)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X105),size(X105),'2341',
     & 1.000,X105,S357)
       deallocate(S357)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S358(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,Q59,D2,S358)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S358)
       deallocate(S358)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S355(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,Q59,D2,S355)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S355)
       deallocate(S355)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M1-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S359(N0+1:M1,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S359)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S359),size(S359),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S359,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S361(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S361)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X55),size(X55),'2134',1.000,
     & X55,S361)
       deallocate(S361)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S359),size(S359),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3214',S359,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S362(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S362)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X56),size(X56),'2134',1.000,
     & X56,S362)
       deallocate(S362)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S359),size(S359),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S359,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S360(M2+1:N3,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S360)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S360)
       deallocate(S360)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S363(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S363)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S363),size(S363),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2314',S363,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S367(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S367)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'3124',-1.000,
     & X4,S367)
       deallocate(S367)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S363),size(S363),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2314',S363,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S364(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S364)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X57),size(X57),'2134',-1.000,
     & X57,S364)
       deallocate(S364)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S365(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S365)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S365),size(S365),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2314',S365,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S366(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S366)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X58),size(X58),'2134',-1.000,
     & X58,S366)
       deallocate(S366)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N0-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S368(M1+1:N2,N0+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S368)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S368),size(S368),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2341',S368,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S370(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S370)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X55),size(X55),'2134',-1.000,
     & X55,S370)
       deallocate(S370)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S368),size(S368),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3241',S368,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S371(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S371)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X56),size(X56),'2134',-1.000,
     & X56,S371)
       deallocate(S371)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S368),size(S368),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2341',S368,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S369(M2+1:N3,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S369)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',1.000,X1,
     & S369)
       deallocate(S369)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,M2-N2/),'2413',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S372(M1+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S372)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S372),size(S372),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'2341',S372,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S378(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S378)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X57),size(X57),'3124',1.000,
     & X57,S378)
       deallocate(S378)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S372),size(S372),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,M1-M1/),'2341',S372,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S379(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S379)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X58),size(X58),'3124',1.000,
     & X58,S379)
       deallocate(S379)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S372),size(S372),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M2-M2,M1-M1/),'3241',S372,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S373(N0+1:M1,N0+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S373)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'3124',1.000,X1,
     & S373)
       deallocate(S373)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'2413',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S374(M1+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S374)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S374),size(S374),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2341',S374,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S380(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S380)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X57),size(X57),'2134',-1.000,
     & X57,S380)
       deallocate(S380)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S374),size(S374),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2341',S374,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S382(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S382)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'3124',-1.000,
     & X4,S382)
       deallocate(S382)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S374),size(S374),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,M1-M1/),'3241',S374,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S375(N0+1:M1,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S375)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X55),size(X55),'3124',-1.000,
     & X55,S375)
       deallocate(S375)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'2413',VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S376(M1+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S376)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S376),size(S376),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,M1-M1/),'3241',S376,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S377(N0+1:M1,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S377)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X56),size(X56),'3124',1.000,
     & X56,S377)
       deallocate(S377)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S376),size(S376),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2341',S376,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S383(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S383)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',1.000,X4,
     & S383)
       deallocate(S383)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S376),size(S376),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2341',S376,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S381(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S381)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X58),size(X58),'2134',-1.000,
     & X58,S381)
       deallocate(S381)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N0-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S384(N0+1:M1,N0+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S384)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S384),size(S384),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S384,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S386(N2+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S386)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X62),size(X62),'2134',-1.000,
     & X62,S386)
       deallocate(S386)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S384),size(S384),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3214',S384,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S387(N2+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S387)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X63),size(X63),'2134',1.000,
     & X63,S387)
       deallocate(S387)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S384),size(S384),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S384,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S385(M2+1:N3,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S385)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X61),size(X61),'2134',-1.000,
     & X61,S385)
       deallocate(S385)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S388(N2+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S388)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S388),size(S388),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2314',S388,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S392(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S392)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'3124',-1.000,
     & X8,S392)
       deallocate(S392)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S388),size(S388),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2314',S388,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S389(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S389)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X64),size(X64),'2134',-1.000,
     & X64,S389)
       deallocate(S389)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S390(N2+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S390)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S390),size(S390),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2314',S390,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S391(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S391)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X65),size(X65),'2134',-1.000,
     & X65,S391)
       deallocate(S391)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,M2-N2/),'2413',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S393(N0+1:M1,N0+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S393)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S393),size(S393),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,N0-N0/),'2341',S393,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S399(N2+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S399)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X64),size(X64),'3124',1.000,
     & X64,S399)
       deallocate(S399)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S393),size(S393),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,N0-N0/),'2341',S393,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S400(N2+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S400)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X65),size(X65),'3124',1.000,
     & X65,S400)
       deallocate(S400)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S393),size(S393),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M2-M2,N0-N0/),'3241',S393,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S394(N0+1:M1,N0+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S394)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'3124',1.000,X5,
     & S394)
       deallocate(S394)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'2413',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S395(N0+1:M1,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S395)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S395),size(S395),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2341',S395,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S401(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S401)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X64),size(X64),'2134',-1.000,
     & X64,S401)
       deallocate(S401)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S395),size(S395),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2341',S395,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S403(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S403)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'3124',-1.000,
     & X8,S403)
       deallocate(S403)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S395),size(S395),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,N0-N0/),'3241',S395,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S396(N0+1:M1,N0+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S396)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X59),size(X59),'3124',-1.000,
     & X59,S396)
       deallocate(S396)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'2413',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S397(N0+1:M1,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S397)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S397),size(S397),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,N0-N0/),'3241',S397,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S398(N0+1:M1,N0+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S398)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X60),size(X60),'3124',1.000,
     & X60,S398)
       deallocate(S398)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S397),size(S397),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2341',S397,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S404(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S404)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',1.000,X8,
     & S404)
       deallocate(S404)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S397),size(S397),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2341',S397,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S402(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S402)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X65),size(X65),'2134',-1.000,
     & X65,S402)
       deallocate(S402)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S405(M1+1:N2,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S405)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S405),size(S405),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S405,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S409(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S409)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X66),size(X66),'3124',1.000,
     & X66,S409)
       deallocate(S409)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z96(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X66,F2,Z96)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & 1.000,V3D,Z96)
       deallocate(Z96)
       deallocate(X66)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S405),size(S405),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S405,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S411(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S411)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X68),size(X68),'3124',1.000,
     & X68,S411)
       deallocate(S411)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z98(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X68,F2,Z98)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z98)
       deallocate(Z98)
       deallocate(X68)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S405),size(S405),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S405,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S406(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S406)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'3124',-1.000,
     & X9,S406)
       deallocate(S406)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M1,N2,X9,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z9(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K5*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X9,F2,Z9)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z9)
       deallocate(Z9)
       deallocate(X9)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S407(M1+1:N2,N0+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S407)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S407),size(S407),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'2341',S407,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S408(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S408)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'3124',-1.000,
     & X10,S408)
       deallocate(S408)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,M1,N2,X10,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z10(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K5*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X10,F2,Z10)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & 1.000,V3D,Z10)
       deallocate(Z10)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S407),size(S407),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'2341',S407,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S412(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S412)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X69),size(X69),'3124',1.000,
     & X69,S412)
       deallocate(S412)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N1-N1,M2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z99(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X69,F2,Z99)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z99)
       deallocate(Z99)
       deallocate(X69)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S407),size(S407),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'2341',S407,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S410(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S410)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X67),size(X67),'3124',1.000,
     & X67,S410)
       deallocate(S410)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N1-N1,M2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z97(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X67,F2,Z97)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & 1.000,V3D,Z97)
       deallocate(Z97)
       deallocate(X67)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S413(N0+1:M1,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S413)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S413),size(S413),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S413,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S421(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S421)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X70),size(X70),'3124',1.000,
     & X70,S421)
       deallocate(S421)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z110(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X70,F2,Z110)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z110)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z110)
       deallocate(Z110)
       deallocate(X70)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S413),size(S413),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S413,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S425(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S425)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X74),size(X74),'3124',1.000,
     & X74,S425)
       deallocate(S425)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z114(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X74,F2,Z114)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z114)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z114)
       deallocate(Z114)
       deallocate(X74)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S413),size(S413),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S413,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S414(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S414)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X13),size(X13),'3124',-1.000,
     & X13,S414)
       deallocate(S414)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,M1,X13,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z13(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X13,F2,Z13)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z13)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z13)
       deallocate(Z13)
       deallocate(X13)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S419(N0+1:M1,N0+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S419)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S419),size(S419),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,N0-N0/),'2341',S419,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S424(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S424)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X73),size(X73),'3124',1.000,
     & X73,S424)
       deallocate(S424)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z113(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X73,F2,Z113)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z113)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z113)
       deallocate(Z113)
       deallocate(X73)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S419),size(S419),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,N0-N0/),'2341',S419,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S428(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S428)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X77),size(X77),'3124',1.000,
     & X77,S428)
       deallocate(S428)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z117(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X77,F2,Z117)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z117)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z117)
       deallocate(Z117)
       deallocate(X77)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S419),size(S419),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,N0-N0/),'2341',S419,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S420(M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S420)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X16),size(X16),'3124',-1.000,
     & X16,S420)
       deallocate(S420)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,N0,M1,X16,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z16(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X16,F2,Z16)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z16)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z16)
       deallocate(Z16)
       deallocate(X16)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S417(N0+1:M1,N0+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S417)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S417),size(S417),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S417,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S418(M2+1:N3,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S418)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X15),size(X15),'3124',-1.000,
     & X15,S418)
       deallocate(S418)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,M2,N3,N0,M1,X15,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z15(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,X15,F2,Z15)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z15)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z15)
       deallocate(Z15)
       deallocate(X15)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S417),size(S417),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S417,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S427(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S427)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X76),size(X76),'3124',1.000,
     & X76,S427)
       deallocate(S427)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z116(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X76,F2,Z116)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z116)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z116)
       deallocate(Z116)
       deallocate(X76)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S417),size(S417),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S417,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S423(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S423)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X72),size(X72),'3124',1.000,
     & X72,S423)
       deallocate(S423)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z112(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X72,F2,Z112)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z112)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z112)
       deallocate(Z112)
       deallocate(X72)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S415(N0+1:M1,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S415)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S415),size(S415),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S415,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S416(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S416)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X14),size(X14),'3124',-1.000,
     & X14,S416)
       deallocate(S416)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,M1,X14,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z14(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X14,F2,Z14)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z14)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z14)
       deallocate(Z14)
       deallocate(X14)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S415),size(S415),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S415,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S422(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S422)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X71),size(X71),'3124',1.000,
     & X71,S422)
       deallocate(S422)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z111(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X71,F2,Z111)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z111)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z111)
       deallocate(Z111)
       deallocate(X71)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S415),size(S415),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S415,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S426(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S426)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X75),size(X75),'3124',1.000,
     & X75,S426)
       deallocate(S426)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N2,N2-N2,N0-N0,M1-M1/),'631245',t3C1,F2)
       allocate(Z115(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X75,F2,Z115)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z115)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z115)
       deallocate(Z115)
       deallocate(X75)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N2-N2/),'2341',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S429(M1+1:N2,N0+1:M1,M1+1:N2,N2+1:N3))
       I1=K4*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S429)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S429),size(S429),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M1-M1,M1-M1/),'4231',S429,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S430(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S430)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X30),size(X30),'3124',1.000,
     & X30,S430)
       deallocate(S430)
C
       call sumx_sorted3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,N0,M1,M1,N2,X30,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,N2-N2,N2-N2,N0-N0/),'461235',t3D,F2)
       allocate(Z30(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K5*K0*K0*K6
       I3=K8*K5
       call EGEMM(I1,I2,I3,X30,F2,Z30)
       deallocate(F2)
C
       V3D=V3D-Z30
       call sum_stripe(6,shape(V3D),size(V3D),'123546',
     & 1.000,V3D,Z30)
       deallocate(Z30)
       deallocate(X30)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,N2-N2/),'2341',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S431(M1+1:N2,M1+1:N2,M1+1:N2,N2+1:N3))
       I1=K4*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S431)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S431),size(S431),shape(D1),size(D1),
     & (/N2-N2,M1-M1,M1-M1,M1-M1/),'4231',S431,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S432(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S432)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X31),size(X31),'3124',1.000,
     & X31,S432)
       deallocate(S432)
C
       call sumx_sorted3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,N0,M1,M1,N2,X31,VCHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,N2-N2,N2-N2,N0-N0/),'561234',t3D,F2)
       allocate(Z31(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K5*K0*K0*K6
       I3=K8*K8
       call EGEMM(I1,I2,I3,X31,F2,Z31)
       deallocate(F2)
C
       V3D=V3D+0.500*Z31
       call sum_stripe(6,shape(V3D),size(V3D),'123546',
     & -0.500,V3D,Z31)
       deallocate(Z31)
       deallocate(X31)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S433(M1+1:N2,N0+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S433)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S433),size(S433),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S433,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S437(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S437)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X78),size(X78),'3124',-1.000,
     & X78,S437)
       deallocate(S437)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z143(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X78,F2,Z143)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z143)
       deallocate(Z143)
       deallocate(X78)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S433),size(S433),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S433,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S434(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S434)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X32),size(X32),'3124',1.000,
     & X32,S434)
       deallocate(S434)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,M2,N3,M1,N2,X32,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z32(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K5*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X32,F2,Z32)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & -1.000,V3D,Z32)
       deallocate(Z32)
       deallocate(X32)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S435(M1+1:N2,N0+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S435)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S435),size(S435),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'2341',S435,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S438(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S438)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X79),size(X79),'3124',-1.000,
     & X79,S438)
       deallocate(S438)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N2,N2-N2,N0-N0,N0-N0/),'621345',t3D,F2)
       allocate(Z144(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X79,F2,Z144)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & 1.000,V3D,Z144)
       deallocate(Z144)
       deallocate(X79)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S435),size(S435),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'2341',S435,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S436(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S436)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X33),size(X33),'3124',1.000,
     & X33,S436)
       deallocate(S436)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,M2,N3,M1,N2,X33,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z33(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K5*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,X33,F2,Z33)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234516',
     & -1.000,V3D,Z33)
       deallocate(Z33)
       deallocate(X33)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S439(M1+1:N2,M1+1:N2,N0+1:N2,M2+1:N3))
       I1=K6*K2*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S439)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S439),size(S439),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'3241',S439,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S440(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S440)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X80),size(X80),'3124',-1.000,
     & X80,S440)
       deallocate(S440)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z145(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X80,F2,Z145)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z145)
       deallocate(Z145)
       deallocate(X80)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S441(M1+1:N2,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S441)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S441),size(S441),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'3241',S441,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S442(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S442)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X81),size(X81),'3124',-1.000,
     & X81,S442)
       deallocate(S442)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N2,N2-N2,N0-N0,N0-N0/),'621345',t3D,F2)
       allocate(Z146(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X81,F2,Z146)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z146)
       deallocate(Z146)
       deallocate(X81)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q60(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q60)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(Q60),size(Q60),shape(B1),size(B1),
     & (/N2-N2,M1-M1/),'21',Q60,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q64(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q64)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X23),size(X23),'21',1.000,
     & X23,Q64)
       deallocate(Q64)
C
       call sumx_sorted21(N0,N2,N0,N2,
     & M1,N2,N0,M1,X23,FBHH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z23(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K5*K0*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X23,F2,Z23)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'123465',
     & -1.000,V3D,Z23)
       call sum_stripe(6,shape(V3D),size(V3D),'123564',
     & 1.000,V3D,Z23)
       deallocate(Z23)
       deallocate(X23)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(Q60),size(Q60),shape(B1),size(B1),
     & (/N2-N2,M1-M1/),'21',Q60,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(Q61(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q61)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X21),size(X21),'21',1.000,
     & X21,Q61)
       deallocate(Q61)
C
       call sumx_sorted21(N0,N2,N0,N2,
     & M1,N2,M1,N2,X21,FBHH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z21(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K5*K0*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X21,F2,Z21)
       deallocate(F2)
C
       V3D=V3D-Z21
       deallocate(Z21)
       deallocate(X21)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N2-N2/),'2341',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S445(N0+1:M1,N0+1:M1,M1+1:N2,N2+1:N3))
       I1=K4*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S445)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S445),size(S445),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M1-M1,N0-N0/),'4231',S445,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S446(N0+1:M1,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S446)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X35),size(X35),'3124',1.000,
     & X35,S446)
       deallocate(S446)
C
       call sumx_sorted3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,N0,M1,N0,M1,X35,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(Z37(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K0*K0*K6
       I3=K8*K5
       call EGEMM(I1,I2,I3,X35,F2,Z37)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'123645',
     & 1.000,V3D,Z37)
       deallocate(Z37)
       deallocate(X35)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,N2-N2/),'2341',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S447(N0+1:M1,M1+1:N2,M1+1:N2,N2+1:N3))
       I1=K4*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S447)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S447),size(S447),shape(D1),size(D1),
     & (/N2-N2,M1-M1,M1-M1,N0-N0/),'4231',S447,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S448(N0+1:M1,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S448)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X36),size(X36),'3124',1.000,
     & X36,S448)
       deallocate(S448)
C
       call sumx_sorted3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,N0,M1,N0,M1,X36,VCHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(Z38(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K0*K0*K6
       I3=K8*K8
       call EGEMM(I1,I2,I3,X36,F2,Z38)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'123645',
     & 0.500,V3D,Z38)
       deallocate(Z38)
       deallocate(X36)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S449(N0+1:M1,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S449)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S449),size(S449),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S449,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S457(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S457)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X82),size(X82),'3124',-1.000,
     & X82,S457)
       deallocate(S457)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z163(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X82,F2,Z163)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z163)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z163)
       deallocate(Z163)
       deallocate(X82)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S449),size(S449),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S449,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S450(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S450)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X37),size(X37),'3124',1.000,
     & X37,S450)
       deallocate(S450)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,M1,X37,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z39(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X37,F2,Z39)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & 1.000,V3D,Z39)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & -1.000,V3D,Z39)
       deallocate(Z39)
       deallocate(X37)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S453(N0+1:M1,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S453)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S453),size(S453),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'2341',S453,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S459(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S459)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X84),size(X84),'3124',-1.000,
     & X84,S459)
       deallocate(S459)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'421356',t3D,F2)
       allocate(Z165(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X84,F2,Z165)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z165)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z165)
       deallocate(Z165)
       deallocate(X84)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S453),size(S453),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'2341',S453,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S454(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S454)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X39),size(X39),'3124',1.000,
     & X39,S454)
       deallocate(S454)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,M2,N3,N0,M1,X39,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z41(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,X39,F2,Z41)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & 1.000,V3D,Z41)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & -1.000,V3D,Z41)
       deallocate(Z41)
       deallocate(X39)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S451(N0+1:M1,N0+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S451)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S451),size(S451),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S451,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S458(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S458)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X83),size(X83),'3124',-1.000,
     & X83,S458)
       deallocate(S458)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z164(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X83,F2,Z164)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z164)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z164)
       deallocate(Z164)
       deallocate(X83)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S451),size(S451),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S451,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S452(M2+1:N3,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S452)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X38),size(X38),'3124',1.000,
     & X38,S452)
       deallocate(S452)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,M1,X38,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z40(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X38,F2,Z40)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z40)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z40)
       deallocate(Z40)
       deallocate(X38)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S455(N0+1:M1,N0+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S455)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S455),size(S455),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,N0-N0/),'2341',S455,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S460(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S460)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X85),size(X85),'3124',-1.000,
     & X85,S460)
       deallocate(S460)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'521346',t3D,F2)
       allocate(Z166(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X85,F2,Z166)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z166)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z166)
       deallocate(Z166)
       deallocate(X85)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S455),size(S455),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,N0-N0/),'2341',S455,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S456(M2+1:N3,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S456)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X40),size(X40),'3124',1.000,
     & X40,S456)
       deallocate(S456)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,M2,N3,N0,M1,X40,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z42(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K5*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,X40,F2,Z42)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234615',
     & -1.000,V3D,Z42)
       call sum_stripe(6,shape(V3D),size(V3D),'235614',
     & 1.000,V3D,Z42)
       deallocate(Z42)
       deallocate(X40)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S461(N0+1:M1,N0+1:M1,N0+1:N2,M2+1:N3))
       I1=K6*K2*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S461)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S461),size(S461),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'3241',S461,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S462(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S462)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X86),size(X86),'3124',-1.000,
     & X86,S462)
       deallocate(S462)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z167(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X86,F2,Z167)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z167)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z167)
       deallocate(Z167)
       deallocate(X86)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S463(N0+1:M1,M1+1:N2,N0+1:N2,M2+1:N3))
       I1=K6*K2*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S463)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S463),size(S463),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'3241',S463,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S464(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S464)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X87),size(X87),'3124',-1.000,
     & X87,S464)
       deallocate(S464)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z168(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X87,F2,Z168)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z168)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z168)
       deallocate(Z168)
       deallocate(X87)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S465(N0+1:M1,N0+1:M1,N0+1:N2,N2+1:M2))
       I1=K0*K2*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S465)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S465),size(S465),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'3241',S465,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S466(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S466)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X88),size(X88),'3124',-1.000,
     & X88,S466)
       deallocate(S466)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'421356',t3D,F2)
       allocate(Z169(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X88,F2,Z169)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z169)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z169)
       deallocate(Z169)
       deallocate(X88)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S467(N0+1:M1,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S467)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S467),size(S467),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,N0-N0/),'3241',S467,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S468(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S468)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X89),size(X89),'3124',-1.000,
     & X89,S468)
       deallocate(S468)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'521346',t3D,F2)
       allocate(Z170(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X89,F2,Z170)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z170)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z170)
       deallocate(Z170)
       deallocate(X89)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q62(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q62)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q62),size(Q62),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q62,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q63(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q63)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X22),size(X22),'21',1.000,
     & X22,Q63)
       deallocate(Q63)
C
       call sumx_sorted21(N0,N2,N0,N2,
     & N0,M1,N0,M1,X22,FBHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z22(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K5*K0*K0*K6
       I3=K5
       call EGEMM(I1,I2,I3,X22,F2,Z22)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'123465',
     & 1.000,V3D,Z22)
       call sum_stripe(6,shape(V3D),size(V3D),'123564',
     & -1.000,V3D,Z22)
       deallocate(Z22)
       deallocate(X22)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'2341',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S443(N0+1:M1,N0+1:M1,N0+1:M1,N2+1:N3))
       I1=K4*K5*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S443)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S443),size(S443),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N0-N0,N0-N0/),'4231',S443,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S444(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S444)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X34),size(X34),'3124',1.000,
     & X34,S444)
       deallocate(S444)
C
       call sumx_sorted3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,N0,M1,N0,M1,N0,M1,X34,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2,N2-N2,M1-M1/),'451236',t3D,F2)
       allocate(Z36(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K0*K0*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,X34,F2,Z36)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'123645',
     & 0.500,V3D,Z36)
       deallocate(Z36)
       deallocate(X34)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S469(N2+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S469)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S469),size(S469),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N2-N2/),'2341',S469,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S470(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S470)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X90),size(X90),'3124',-1.000,
     & X90,S470)
       deallocate(S470)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,M2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z185(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K5*K5*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,X90,F2,Z185)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345612',
     & -0.500,V3D,Z185)
       deallocate(Z185)
       deallocate(X90)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S471(N2+1:M2,N0+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S471)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S471),size(S471),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,N2-N2/),'2341',S471,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S481(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S481)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X44),size(X44),'4123',-1.000,
     & X44,S481)
       deallocate(S481)
C
       call sumx_sorted1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,N2,M2,N2,M2,N2,M2,X44,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,M2-N2,N0-N0,N0-N0,M1-M1/),'132456',t3D,F2)
       allocate(Z50(M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K5*K5*K6
       I3=K0*K6
       call EGEMM(I1,I2,I3,X44,F2,Z50)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'145623',
     & -1.000,V3D,Z50)
       deallocate(Z50)
       deallocate(X44)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S471),size(S471),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,N2-N2/),'2341',S471,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S472(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S472)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X91),size(X91),'3124',-1.000,
     & X91,S472)
       deallocate(S472)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z186(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K5*K5*K0
       I3=K0*K6
       call EGEMM(I1,I2,I3,X91,F2,Z186)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345612',
     & -1.000,V3D,Z186)
       deallocate(Z186)
       deallocate(X91)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S475(N2+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S475)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S475),size(S475),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N2-N2/),'2341',S475,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S476(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S476)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X93),size(X93),'3124',-1.000,
     & X93,S476)
       deallocate(S476)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,M2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z188(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K5*K5*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,X93,F2,Z188)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & 0.500,V3D,Z188)
       deallocate(Z188)
       deallocate(X93)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S477(N2+1:M2,N0+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S477)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S477),size(S477),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,N2-N2/),'2341',S477,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S478(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S478)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X94),size(X94),'3124',-1.000,
     & X94,S478)
       deallocate(S478)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z189(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K5*K5*K0
       I3=K0*K6
       call EGEMM(I1,I2,I3,X94,F2,Z189)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & 1.000,V3D,Z189)
       deallocate(Z189)
       deallocate(X94)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S479(N2+1:M2,N0+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S479)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S479),size(S479),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2341',S479,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S480(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S480)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X95),size(X95),'3124',-1.000,
     & X95,S480)
       deallocate(S480)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z190(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K5*K5*K0
       I3=K0*K0
       call EGEMM(I1,I2,I3,X95,F2,Z190)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'245613',
     & 0.500,V3D,Z190)
       deallocate(Z190)
       deallocate(X95)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q65(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q69(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q65,B2,Q69)
       deallocate(B2)
C
       call sum_stripe(2,shape(X26),size(X26),'21',-1.000,
     & X26,Q69)
       deallocate(Q69)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & M2,N3,N2,M2,X26,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,M2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z26(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K5*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,X26,F2,Z26)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134562',
     & -1.000,V3D,Z26)
       deallocate(Z26)
       deallocate(X26)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q71(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q65,B2,Q71)
       deallocate(B2)
C
       call sum_stripe(2,shape(X28),size(X28),'21',-1.000,
     & X28,Q71)
       deallocate(Q71)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & M2,N3,N2,M2,X28,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,M2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z28(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K5*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,X28,F2,Z28)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124563',
     & 1.000,V3D,Z28)
       deallocate(Z28)
       deallocate(X28)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q66(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q65,B2,Q66)
       deallocate(B2)
C
       call sum_stripe(2,shape(X24),size(X24),'21',-1.000,
     & X24,Q66)
       deallocate(Q66)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & M2,N3,M2,N3,X24,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z24(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K5*K5*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,X24,F2,Z24)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234561',
     & 1.000,V3D,Z24)
       deallocate(Z24)
       deallocate(X24)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S473(N2+1:M2,N0+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S473)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S473),size(S473),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2341',S473,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S474(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S474)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X92),size(X92),'3124',-1.000,
     & X92,S474)
       deallocate(S474)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z187(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K5*K5*K0
       I3=K0*K0
       call EGEMM(I1,I2,I3,X92,F2,Z187)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345612',
     & -0.500,V3D,Z187)
       deallocate(Z187)
       deallocate(X92)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S473),size(S473),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2341',S473,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S482(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S482)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X45),size(X45),'4123',-1.000,
     & X45,S482)
       deallocate(S482)
C
       call sumx_sorted1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,M2,N2,M2,N2,M2,N2,M2,X45,VCAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,M2-N2,N0-N0,N0-N0,M1-M1/),'231456',t3D,F2)
       allocate(Z51(M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K5*K5*K6
       I3=K0*K0
       call EGEMM(I1,I2,I3,X45,F2,Z51)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'145623',
     & 0.500,V3D,Z51)
       deallocate(Z51)
       deallocate(X45)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q67(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(Q68(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q67,B2,Q68)
       deallocate(B2)
C
       call sum_stripe(2,shape(X25),size(X25),'21',-1.000,
     & X25,Q68)
       deallocate(Q68)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & N2,M2,M2,N3,X25,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'123456',t3D,F2)
       allocate(Z25(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K5*K5*K0*K0
       I3=K0
       call EGEMM(I1,I2,I3,X25,F2,Z25)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234561',
     & 1.000,V3D,Z25)
       deallocate(Z25)
       deallocate(X25)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q72(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q67,B2,Q72)
       deallocate(B2)
C
       call sum_stripe(2,shape(X29),size(X29),'21',-1.000,
     & X29,Q72)
       deallocate(Q72)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & N2,M2,N2,M2,X29,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,M2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'213456',t3D,F2)
       allocate(Z29(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K5*K0*K6
       I3=K0
       call EGEMM(I1,I2,I3,X29,F2,Z29)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124563',
     & -1.000,V3D,Z29)
       deallocate(Z29)
       deallocate(X29)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q70(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q67,B2,Q70)
       deallocate(B2)
C
       call sum_stripe(2,shape(X27),size(X27),'21',-1.000,
     & X27,Q70)
       deallocate(Q70)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & N2,M2,N2,M2,X27,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N2-N2,M2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'213456',t3D,F2)
       allocate(Z27(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K5*K5*K0*K6
       I3=K0
       call EGEMM(I1,I2,I3,X27,F2,Z27)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134562',
     & 1.000,V3D,Z27)
       deallocate(Z27)
       deallocate(X27)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S483(M1+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S483)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S483),size(S483),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'3421',S483,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S485(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S485)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X55),size(X55),'2314',-1.000,
     & X55,S485)
       deallocate(S485)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S483),size(S483),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'3421',S483,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S486(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S486)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X56),size(X56),'2314',1.000,
     & X56,S486)
       deallocate(S486)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S483),size(S483),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'3421',S483,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S484(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S484)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S484)
       deallocate(S484)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S487(N0+1:M1,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S487)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S487),size(S487),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'3421',S487,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S489(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S489)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X55),size(X55),'2413',1.000,
     & X55,S489)
       deallocate(S489)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S487),size(S487),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'3421',S487,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S490(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S490)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X56),size(X56),'2413',-1.000,
     & X56,S490)
       deallocate(S490)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S487),size(S487),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'3421',S487,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S501(M2+1:N3,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S501)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X61),size(X61),'2314',1.000,
     & X61,S501)
       deallocate(S501)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S487),size(S487),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'3421',S487,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S502(N2+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S502)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X62),size(X62),'2314',1.000,
     & X62,S502)
       deallocate(S502)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S487),size(S487),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'3421',S487,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S503(N2+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S503)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X63),size(X63),'2314',1.000,
     & X63,S503)
       deallocate(S503)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S487),size(S487),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'3421',S487,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S488(M2+1:N3,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S488)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2413',-1.000,
     & X1,S488)
       deallocate(S488)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S497(N2+1:M2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S497)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S497),size(S497),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N2-N2/),'2431',S497,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S509(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S509)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X65),size(X65),'2413',-1.000,
     & X65,S509)
       deallocate(S509)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S497),size(S497),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N2-N2/),'2431',S497,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S511(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S511)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2413',1.000,X8,
     & S511)
       deallocate(S511)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S497),size(S497),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N2-N2/),'2431',S497,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S498(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S498)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X58),size(X58),'2413',-1.000,
     & X58,S498)
       deallocate(S498)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S491(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S491)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S491),size(S491),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S491,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S500(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S500)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'3124',1.000,X4,
     & S500)
       deallocate(S500)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S491),size(S491),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S491,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S492(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S492)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X57),size(X57),'2134',1.000,
     & X57,S492)
       deallocate(S492)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S495(N2+1:M2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S495)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S495),size(S495),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N2-N2/),'2431',S495,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S508(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S508)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X64),size(X64),'2413',-1.000,
     & X64,S508)
       deallocate(S508)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S495),size(S495),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N2-N2/),'2431',S495,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S510(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S510)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'3412',-1.000,
     & X8,S510)
       deallocate(S510)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S495),size(S495),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N2-N2/),'2431',S495,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S496(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S496)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X57),size(X57),'2413',-1.000,
     & X57,S496)
       deallocate(S496)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S504(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S504)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S504),size(S504),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S504,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S505(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S505)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X64),size(X64),'2134',1.000,
     & X64,S505)
       deallocate(S505)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'4213',t2B,D2)
       allocate(S493(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S493)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S493),size(S493),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S493,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S494(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S494)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X58),size(X58),'2134',1.000,
     & X58,S494)
       deallocate(S494)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S493),size(S493),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S493,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S499(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S499)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',-1.000,
     & X4,S499)
       deallocate(S499)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S506(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S506)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S506),size(S506),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S506,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S507(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S507)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X65),size(X65),'2134',1.000,
     & X65,S507)
       deallocate(S507)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,M1-N0/),'12',t1B,B2)
       allocate(S512(M1+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S512)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S512),size(S512),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'2341',S512,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S514(M2+1:N3,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S514)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X102),size(X102),'2314',
     & -1.000,X102,S514)
       deallocate(S514)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(Z280(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X102,D2,Z280)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'245136',
     & 0.500,V3D,Z280)
       call sum_stripe(6,shape(V3D),size(V3D),'345126',
     & -0.500,V3D,Z280)
       deallocate(Z280)
       deallocate(X102)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S512),size(S512),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'3421',S512,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S515(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S515)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X56),size(X56),'2314',1.000,
     & X56,S515)
       deallocate(S515)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S512),size(S512),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S512,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S516(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S516)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X55),size(X55),'2314',1.000,
     & X55,S516)
       deallocate(S516)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S512),size(S512),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S512,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S517(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S517)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',-1.000,
     & X1,S517)
       deallocate(S517)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder_shift(4,shape(S512),size(S512),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N0-N0,M1-M1/),'4231',S512,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S555(N0+1:M1,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S555)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S555),size(S555),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S555,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S556(M2+1:N3,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S556)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',1.000,X1,
     & S556)
       deallocate(S556)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S512),size(S512),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'3241',S512,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S559(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S559)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S559),size(S559),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2314',S559,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S560(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S560)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X57),size(X57),'2134',1.000,
     & X57,S560)
       deallocate(S560)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S512),size(S512),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'3241',S512,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S561(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S561)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S561),size(S561),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2314',S561,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S562(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S562)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X58),size(X58),'2134',1.000,
     & X58,S562)
       deallocate(S562)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(S512),size(S512),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'2341',S512,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S513(N2+1:M2,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S513)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2314',-0.500,
     & X4,S513)
       deallocate(S513)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S555),size(S555),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S555,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S557(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S557)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X55),size(X55),'2134',-1.000,
     & X55,S557)
       deallocate(S557)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S555),size(S555),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3214',S555,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S558(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S558)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X56),size(X56),'2134',-1.000,
     & X56,S558)
       deallocate(S558)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S561),size(S561),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2314',S561,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S563(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S563)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',-1.000,
     & X4,S563)
       deallocate(S563)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S518(N0+1:M1,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S518)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S518),size(S518),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'3421',S518,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S526(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S526)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X56),size(X56),'2413',-1.000,
     & X56,S526)
       deallocate(S526)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S518),size(S518),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'2341',S518,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S519(N2+1:M2,N2+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S519)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2314',-0.500,
     & X8,S519)
       deallocate(S519)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S518),size(S518),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'2341',S518,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S520(M2+1:N3,N2+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S520)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X104),size(X104),'2314',
     & -1.000,X104,S520)
       deallocate(S520)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z284(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K6
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X104,D2,Z284)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'246135',
     & -0.500,V3D,Z284)
       call sum_stripe(6,shape(V3D),size(V3D),'346125',
     & 0.500,V3D,Z284)
       call sum_stripe(6,shape(V3D),size(V3D),'256134',
     & 0.500,V3D,Z284)
       call sum_stripe(6,shape(V3D),size(V3D),'356124',
     & -0.500,V3D,Z284)
       deallocate(Z284)
       deallocate(X104)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S518),size(S518),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'3421',S518,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S521(N2+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S521)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X63),size(X63),'2314',1.000,
     & X63,S521)
       deallocate(S521)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z84(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X63,D2,Z84)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & -1.000,V3D,Z84)
       call sum_stripe(6,shape(V3D),size(V3D),'126354',
     & 1.000,V3D,Z84)
       deallocate(Z84)
       deallocate(X63)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S518),size(S518),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S518,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S522(N2+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S522)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X62),size(X62),'2314',-1.000,
     & X62,S522)
       deallocate(S522)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z83(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X62,D2,Z83)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & 1.000,V3D,Z83)
       call sum_stripe(6,shape(V3D),size(V3D),'136254',
     & -1.000,V3D,Z83)
       deallocate(Z83)
       deallocate(X62)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S518),size(S518),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S518,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S523(M2+1:N3,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S523)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2413',1.000,X1,
     & S523)
       deallocate(S523)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S518),size(S518),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S518,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S524(M2+1:N3,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S524)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X61),size(X61),'2314',-1.000,
     & X61,S524)
       deallocate(S524)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z82(N2+1:M2,N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5*K5*K6
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X61,D2,Z82)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'236145',
     & -1.000,V3D,Z82)
       call sum_stripe(6,shape(V3D),size(V3D),'236154',
     & 1.000,V3D,Z82)
       deallocate(Z82)
       deallocate(X61)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S518),size(S518),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N0-N0,N0-N0/),'4231',S518,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S564(N0+1:M1,N0+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S564)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S564),size(S564),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S564,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S565(M2+1:N3,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S565)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',1.000,X5,
     & S565)
       deallocate(S565)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S518),size(S518),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'3241',S518,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S568(N2+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S568)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S568),size(S568),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2314',S568,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S569(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S569)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X64),size(X64),'2134',1.000,
     & X64,S569)
       deallocate(S569)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S518),size(S518),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'3241',S518,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S570(N2+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S570)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S570),size(S570),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2314',S570,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S571(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S571)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X65),size(X65),'2134',1.000,
     & X65,S571)
       deallocate(S571)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S518),size(S518),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S518,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S525(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S525)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X55),size(X55),'2413',-1.000,
     & X55,S525)
       deallocate(S525)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S564),size(S564),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S564,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S566(N2+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S566)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X59),size(X59),'2134',-1.000,
     & X59,S566)
       deallocate(S566)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S564),size(S564),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3214',S564,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S567(N2+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S567)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X60),size(X60),'2134',-1.000,
     & X60,S567)
       deallocate(S567)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S570),size(S570),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2314',S570,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S572(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S572)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',-1.000,
     & X8,S572)
       deallocate(S572)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4132',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S544(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S544)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S544),size(S544),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S544,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S546(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S546)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X58),size(X58),'2134',-1.000,
     & X58,S546)
       deallocate(S546)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(S544),size(S544),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S544,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S545(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S545)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X57),size(X57),'2134',-1.000,
     & X57,S545)
       deallocate(S545)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S535(N2+1:M2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S535)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S535),size(S535),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2341',S535,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S548(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S548)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X57),size(X57),'2413',-1.000,
     & X57,S548)
       deallocate(S548)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(Z67(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X57,D2,Z67)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345126',
     & 1.000,V3D,Z67)
       deallocate(Z67)
       deallocate(X57)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S535),size(S535),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2431',S535,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S552(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S552)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'3412',1.000,X4,
     & S552)
       deallocate(S552)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S535),size(S535),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2341',S535,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S536(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S536)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X64),size(X64),'2413',-1.000,
     & X64,S536)
       deallocate(S536)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(S535),size(S535),shape(D1),size(D1),
     & (/N2-N2,N2-N2,N0-N0,N2-N2/),'3421',S535,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S547(N0+1:M1,N0+1:M1,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K5*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S547)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X59),size(X59),'3412',-0.500,
     & X59,S547)
       deallocate(S547)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z80(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X59,D2,Z80)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & -1.000,V3D,Z80)
       deallocate(Z80)
       deallocate(X59)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S538(N2+1:M2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S538)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(S538),size(S538),shape(D1),size(D1),
     & (/N2-N2,N2-N2,N0-N0,N2-N2/),'3421',S538,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S549(N0+1:M1,N0+1:M1,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K5*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S549)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X60),size(X60),'3412',0.500,
     & X60,S549)
       deallocate(S549)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z81(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X60,D2,Z81)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & -1.000,V3D,Z81)
       deallocate(Z81)
       deallocate(X60)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S538),size(S538),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2431',S538,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S553(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S553)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2413',-1.000,
     & X4,S553)
       deallocate(S553)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S538),size(S538),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2341',S538,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S539(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S539)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X65),size(X65),'2413',-1.000,
     & X65,S539)
       deallocate(S539)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S538),size(S538),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2341',S538,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S551(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S551)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X58),size(X58),'2413',-1.000,
     & X58,S551)
       deallocate(S551)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(Z68(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X58,D2,Z68)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'245136',
     & -1.000,V3D,Z68)
       deallocate(Z68)
       deallocate(X58)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S527(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S527)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S527,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S529(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S529)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X64),size(X64),'2134',1.000,
     & X64,S529)
       deallocate(S529)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z85(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K6
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X64,D2,Z85)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'346125',
     & -1.000,V3D,Z85)
       call sum_stripe(6,shape(V3D),size(V3D),'356124',
     & 1.000,V3D,Z85)
       deallocate(Z85)
       deallocate(X64)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S527,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S532(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S532)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',-1.000,
     & X8,S532)
       deallocate(S532)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S527,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S533(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S533)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'3124',1.000,X8,
     & S533)
       deallocate(S533)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S527,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S528(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S528)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X65),size(X65),'2134',1.000,
     & X65,S528)
       deallocate(S528)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z86(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K6
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X65,D2,Z86)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'246135',
     & 1.000,V3D,Z86)
       call sum_stripe(6,shape(V3D),size(V3D),'256134',
     & -1.000,V3D,Z86)
       deallocate(Z86)
       deallocate(X65)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S530(N0+1:M1,M1+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S530)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S530),size(S530),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3412',S530,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S537(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S537)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X55),size(X55),'2134',0.500,
     & X55,S537)
       deallocate(S537)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(Z62(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K5*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X55,D2,Z62)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134256',
     & -1.000,V3D,Z62)
       call sum_stripe(6,shape(V3D),size(V3D),'135246',
     & 1.000,V3D,Z62)
       deallocate(Z62)
       deallocate(X55)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S530),size(S530),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'4312',S530,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S541(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S541)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X56),size(X56),'2134',0.500,
     & X56,S541)
       deallocate(S541)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(Z63(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K5*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X56,D2,Z63)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124356',
     & -1.000,V3D,Z63)
       call sum_stripe(6,shape(V3D),size(V3D),'125346',
     & 1.000,V3D,Z63)
       deallocate(Z63)
       deallocate(X56)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(S530),size(S530),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3412',S530,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S531(M2+1:N3,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S531)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-0.500,
     & X1,S531)
       deallocate(S531)
C
       call sumx_sorted2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M2,N3,N0,M1,M1,N2,X1,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(Z1(N2+1:M2,N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1,M1+1:N2))
       I1=K8*K5*K6
       I2=K5*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X1,D2,Z1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234156',
     & -1.000,V3D,Z1)
       call sum_stripe(6,shape(V3D),size(V3D),'235146',
     & 1.000,V3D,Z1)
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q73(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(S554(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,Q73,D2,S554)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S554)
       deallocate(S554)
C
       call sumx_sorted2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,M2,N2,M2,M1,N2,X4,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,M2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(Z4(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K5*K5*K6
       I3=K4
       call EGEMM(I1,I2,I3,X4,D2,Z4)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'145236',
     & -1.000,V3D,Z4)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q73),size(Q73),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q73,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(S540(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S540)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S540)
       deallocate(S540)
C
       call sumx_sorted2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,M2,N0,M1,M1,N2,X2,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(Z2(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K5*K0*K6
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
       deallocate(Z2)
       deallocate(X2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q73),size(Q73),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q73,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S550(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S550)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X105),size(X105),'2341',
     & 1.000,X105,S550)
       deallocate(S550)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,M2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z311(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X105,D2,Z311)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & 1.000,V3D,Z311)
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & -1.000,V3D,Z311)
       deallocate(Z311)
       deallocate(X105)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S534(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,Q73,D2,S534)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S534)
       deallocate(S534)
C
       call sumx_sorted2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,M2,N2,M2,N0,M1,X8,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,M2-N2,N0-N0,M1-N0/),'1234',t2C,D2)
       allocate(Z8(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K0
       I2=K8*K5*K6
       I3=K4
       call EGEMM(I1,I2,I3,X8,D2,Z8)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'146235',
     & 1.000,V3D,Z8)
       call sum_stripe(6,shape(V3D),size(V3D),'156234',
     & -1.000,V3D,Z8)
       deallocate(Z8)
       deallocate(X8)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S542(N0+1:M1,N0+1:M1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K5*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S542)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S542),size(S542),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3412',S542,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,M2-N2/),'21',t1B,B2)
       allocate(S543(M2+1:N3,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S543)
       deallocate(D1)
       deallocate(B2)
       deallocate(S542)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',-0.500,
     & X5,S543)
       deallocate(S543)
C
       call sumx_sorted2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M2,N3,N0,M1,N0,M1,X5,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,M1-N0/),'3124',t2C,D2)
       allocate(Z5(N2+1:M2,N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5*K5*K6
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X5,D2,Z5)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'236145',
     & -1.000,V3D,Z5)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z34(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z34)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & 1.000,V3D,Z34)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & -1.000,V3D,Z34)
       deallocate(Z34)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,M1-N0/),'4132',VCHPHP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N2,N2-N2,N0-N0,N0-N0/),'621345',t3D,F2)
       allocate(Z35(M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z35)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134526',
     & -1.000,V3D,Z35)
       call sum_stripe(6,shape(V3D),size(V3D),'124536',
     & 1.000,V3D,Z35)
       deallocate(Z35)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N0-N0/),'4132',VCHPHP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'412356',t3D,F2)
       allocate(Z43(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z43)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z43)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z43)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z43)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z43)
       deallocate(Z43)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'4132',VCHPHP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'512346',t3D,F2)
       allocate(Z44(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z44)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z44)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z44)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z44)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z44)
       deallocate(Z44)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'4132',VCHPHP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'421356',t3D,F2)
       allocate(Z45(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z45)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & 1.000,V3D,Z45)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & -1.000,V3D,Z45)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & -1.000,V3D,Z45)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & 1.000,V3D,Z45)
       deallocate(Z45)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'4132',VCHPHP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N2,N2-N2,N0-N0,M1-M1/),'521346',t3D,F2)
       allocate(Z46(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z46)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'134625',
     & -1.000,V3D,Z46)
       call sum_stripe(6,shape(V3D),size(V3D),'124635',
     & 1.000,V3D,Z46)
       call sum_stripe(6,shape(V3D),size(V3D),'135624',
     & 1.000,V3D,Z46)
       call sum_stripe(6,shape(V3D),size(V3D),'125634',
     & -1.000,V3D,Z46)
       deallocate(Z46)
C
       call sumx_sorted3(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,N2,M2,N2,M2,N0,M1,N0,M1,M1,N2,HT3D,V3D,1.0)
       deallocate(V3D)
C
       end
