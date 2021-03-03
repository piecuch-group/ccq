       subroutine t3C100111_update(N0,N1,N2,N3,HT3C4,shift,
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
       real*8 HT3C4(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N1)
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
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::S7(:,:,:,:)
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::Q10(:,:)
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
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::S62(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::S64(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::S66(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::S68(:,:,:,:)
       real*8,allocatable::Q17(:,:)
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
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::Q19(:,:)
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
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::Q21(:,:)
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
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::S103(:,:,:,:)
       real*8,allocatable::S104(:,:,:,:)
       real*8,allocatable::S105(:,:,:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::S106(:,:,:,:)
       real*8,allocatable::S107(:,:,:,:)
       real*8,allocatable::S108(:,:,:,:)
       real*8,allocatable::S109(:,:,:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q28(:,:)
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
       real*8,allocatable::Q29(:,:)
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
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::S244(:,:,:,:)
       real*8,allocatable::S245(:,:,:,:)
       real*8,allocatable::S246(:,:,:,:)
       real*8,allocatable::S247(:,:,:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::S248(:,:,:,:)
       real*8,allocatable::S249(:,:,:,:)
       real*8,allocatable::S250(:,:,:,:)
       real*8,allocatable::S251(:,:,:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q36(:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::S252(:,:,:,:)
       real*8,allocatable::S253(:,:,:,:)
       real*8,allocatable::S254(:,:,:,:)
       real*8,allocatable::S255(:,:,:,:)
       real*8,allocatable::S256(:,:,:,:)
       real*8,allocatable::S258(:,:,:,:)
       real*8,allocatable::S257(:,:,:,:)
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
       real*8,allocatable::Q38(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::S322(:,:,:,:)
       real*8,allocatable::S323(:,:,:,:)
       real*8,allocatable::S324(:,:,:,:)
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::S325(:,:,:,:)
       real*8,allocatable::S337(:,:,:,:)
       real*8,allocatable::S338(:,:,:,:)
       real*8,allocatable::S326(:,:,:,:)
       real*8,allocatable::S329(:,:,:,:)
       real*8,allocatable::S330(:,:,:,:)
       real*8,allocatable::S331(:,:,:,:)
       real*8,allocatable::S332(:,:,:,:)
       real*8,allocatable::Q44(:,:)
       real*8,allocatable::Q45(:,:)
       real*8,allocatable::Q46(:,:)
       real*8,allocatable::Q47(:,:)
       real*8,allocatable::Q48(:,:)
       real*8,allocatable::Q49(:,:)
       real*8,allocatable::S333(:,:,:,:)
       real*8,allocatable::S334(:,:,:,:)
       real*8,allocatable::S335(:,:,:,:)
       real*8,allocatable::S336(:,:,:,:)
       real*8,allocatable::S327(:,:,:,:)
       real*8,allocatable::S339(:,:,:,:)
       real*8,allocatable::S340(:,:,:,:)
       real*8,allocatable::S328(:,:,:,:)
       real*8,allocatable::S341(:,:,:,:)
       real*8,allocatable::S343(:,:,:,:)
       real*8,allocatable::S342(:,:,:,:)
       real*8,allocatable::S344(:,:,:,:)
       real*8,allocatable::S345(:,:,:,:)
       real*8,allocatable::S346(:,:,:,:)
       real*8,allocatable::S347(:,:,:,:)
       real*8,allocatable::S348(:,:,:,:)
       real*8,allocatable::S417(:,:,:,:)
       real*8,allocatable::S418(:,:,:,:)
       real*8,allocatable::S349(:,:,:,:)
       real*8,allocatable::S353(:,:,:,:)
       real*8,allocatable::S355(:,:,:,:)
       real*8,allocatable::S356(:,:,:,:)
       real*8,allocatable::S354(:,:,:,:)
       real*8,allocatable::S357(:,:,:,:)
       real*8,allocatable::S358(:,:,:,:)
       real*8,allocatable::S359(:,:,:,:)
       real*8,allocatable::S360(:,:,:,:)
       real*8,allocatable::S361(:,:,:,:)
       real*8,allocatable::S362(:,:,:,:)
       real*8,allocatable::S363(:,:,:,:)
       real*8,allocatable::S367(:,:,:,:)
       real*8,allocatable::S364(:,:,:,:)
       real*8,allocatable::S365(:,:,:,:)
       real*8,allocatable::S368(:,:,:,:)
       real*8,allocatable::S366(:,:,:,:)
       real*8,allocatable::Q50(:,:)
       real*8,allocatable::Q51(:,:)
       real*8,allocatable::S369(:,:,:,:)
       real*8,allocatable::S370(:,:,:,:)
       real*8,allocatable::S371(:,:,:,:)
       real*8,allocatable::S372(:,:,:,:)
       real*8,allocatable::S373(:,:,:,:)
       real*8,allocatable::S374(:,:,:,:)
       real*8,allocatable::S375(:,:,:,:)
       real*8,allocatable::S376(:,:,:,:)
       real*8,allocatable::Q52(:,:)
       real*8,allocatable::Q53(:,:)
       real*8,allocatable::Q54(:,:)
       real*8,allocatable::Q55(:,:)
       real*8,allocatable::S377(:,:,:,:)
       real*8,allocatable::S385(:,:,:,:)
       real*8,allocatable::S378(:,:,:,:)
       real*8,allocatable::S381(:,:,:,:)
       real*8,allocatable::S387(:,:,:,:)
       real*8,allocatable::S382(:,:,:,:)
       real*8,allocatable::Q56(:,:)
       real*8,allocatable::Q60(:,:)
       real*8,allocatable::Q57(:,:)
       real*8,allocatable::S379(:,:,:,:)
       real*8,allocatable::S386(:,:,:,:)
       real*8,allocatable::S380(:,:,:,:)
       real*8,allocatable::S383(:,:,:,:)
       real*8,allocatable::S388(:,:,:,:)
       real*8,allocatable::S384(:,:,:,:)
       real*8,allocatable::Q58(:,:)
       real*8,allocatable::Q61(:,:)
       real*8,allocatable::Q59(:,:)
       real*8,allocatable::Q62(:,:)
       real*8,allocatable::Q63(:,:)
       real*8,allocatable::Q64(:,:)
       real*8,allocatable::Q65(:,:)
       real*8,allocatable::S389(:,:,:,:)
       real*8,allocatable::S391(:,:,:,:)
       real*8,allocatable::S390(:,:,:,:)
       real*8,allocatable::S392(:,:,:,:)
       real*8,allocatable::S393(:,:,:,:)
       real*8,allocatable::Q66(:,:)
       real*8,allocatable::S397(:,:,:,:)
       real*8,allocatable::S394(:,:,:,:)
       real*8,allocatable::S395(:,:,:,:)
       real*8,allocatable::S396(:,:,:,:)
       real*8,allocatable::S398(:,:,:,:)
       real*8,allocatable::S400(:,:,:,:)
       real*8,allocatable::S401(:,:,:,:)
       real*8,allocatable::S402(:,:,:,:)
       real*8,allocatable::S403(:,:,:,:)
       real*8,allocatable::S509(:,:,:,:)
       real*8,allocatable::S510(:,:,:,:)
       real*8,allocatable::S511(:,:,:,:)
       real*8,allocatable::S512(:,:,:,:)
       real*8,allocatable::S399(:,:,:,:)
       real*8,allocatable::S514(:,:,:,:)
       real*8,allocatable::S515(:,:,:,:)
       real*8,allocatable::S513(:,:,:,:)
       real*8,allocatable::Q67(:,:)
       real*8,allocatable::S416(:,:,:,:)
       real*8,allocatable::S410(:,:,:,:)
       real*8,allocatable::S413(:,:,:,:)
       real*8,allocatable::S350(:,:,:,:)
       real*8,allocatable::S352(:,:,:,:)
       real*8,allocatable::S351(:,:,:,:)
       real*8,allocatable::S419(:,:,:,:)
       real*8,allocatable::S421(:,:,:,:)
       real*8,allocatable::S420(:,:,:,:)
       real*8,allocatable::S422(:,:,:,:)
       real*8,allocatable::S424(:,:,:,:)
       real*8,allocatable::S423(:,:,:,:)
       real*8,allocatable::S425(:,:,:,:)
       real*8,allocatable::S426(:,:,:,:)
       real*8,allocatable::S427(:,:,:,:)
       real*8,allocatable::S431(:,:,:,:)
       real*8,allocatable::S428(:,:,:,:)
       real*8,allocatable::S429(:,:,:,:)
       real*8,allocatable::S432(:,:,:,:)
       real*8,allocatable::S430(:,:,:,:)
       real*8,allocatable::S433(:,:,:,:)
       real*8,allocatable::S441(:,:,:,:)
       real*8,allocatable::S434(:,:,:,:)
       real*8,allocatable::S437(:,:,:,:)
       real*8,allocatable::S443(:,:,:,:)
       real*8,allocatable::S438(:,:,:,:)
       real*8,allocatable::S435(:,:,:,:)
       real*8,allocatable::S442(:,:,:,:)
       real*8,allocatable::S436(:,:,:,:)
       real*8,allocatable::S439(:,:,:,:)
       real*8,allocatable::S444(:,:,:,:)
       real*8,allocatable::S440(:,:,:,:)
       real*8,allocatable::S445(:,:,:,:)
       real*8,allocatable::S446(:,:,:,:)
       real*8,allocatable::S447(:,:,:,:)
       real*8,allocatable::S448(:,:,:,:)
       real*8,allocatable::S449(:,:,:,:)
       real*8,allocatable::S450(:,:,:,:)
       real*8,allocatable::S451(:,:,:,:)
       real*8,allocatable::S459(:,:,:,:)
       real*8,allocatable::S452(:,:,:,:)
       real*8,allocatable::S455(:,:,:,:)
       real*8,allocatable::S461(:,:,:,:)
       real*8,allocatable::S456(:,:,:,:)
       real*8,allocatable::S453(:,:,:,:)
       real*8,allocatable::S460(:,:,:,:)
       real*8,allocatable::S454(:,:,:,:)
       real*8,allocatable::S457(:,:,:,:)
       real*8,allocatable::S462(:,:,:,:)
       real*8,allocatable::S458(:,:,:,:)
       real*8,allocatable::Q68(:,:)
       real*8,allocatable::Q69(:,:)
       real*8,allocatable::Q70(:,:)
       real*8,allocatable::Q71(:,:)
       real*8,allocatable::S463(:,:,:,:)
       real*8,allocatable::S464(:,:,:,:)
       real*8,allocatable::S465(:,:,:,:)
       real*8,allocatable::S466(:,:,:,:)
       real*8,allocatable::S467(:,:,:,:)
       real*8,allocatable::S468(:,:,:,:)
       real*8,allocatable::Q72(:,:)
       real*8,allocatable::Q76(:,:)
       real*8,allocatable::Q73(:,:)
       real*8,allocatable::Q74(:,:)
       real*8,allocatable::Q77(:,:)
       real*8,allocatable::Q75(:,:)
       real*8,allocatable::S469(:,:,:,:)
       real*8,allocatable::S474(:,:,:,:)
       real*8,allocatable::S475(:,:,:,:)
       real*8,allocatable::S476(:,:,:,:)
       real*8,allocatable::S478(:,:,:,:)
       real*8,allocatable::S516(:,:,:,:)
       real*8,allocatable::S517(:,:,:,:)
       real*8,allocatable::S470(:,:,:,:)
       real*8,allocatable::S518(:,:,:,:)
       real*8,allocatable::S471(:,:,:,:)
       real*8,allocatable::S472(:,:,:,:)
       real*8,allocatable::S473(:,:,:,:)
       real*8,allocatable::S480(:,:,:,:)
       real*8,allocatable::S485(:,:,:,:)
       real*8,allocatable::S481(:,:,:,:)
       real*8,allocatable::S408(:,:,:,:)
       real*8,allocatable::S489(:,:,:,:)
       real*8,allocatable::S409(:,:,:,:)
       real*8,allocatable::S484(:,:,:,:)
       real*8,allocatable::S487(:,:,:,:)
       real*8,allocatable::S488(:,:,:,:)
       real*8,allocatable::S482(:,:,:,:)
       real*8,allocatable::S483(:,:,:,:)
       real*8,allocatable::Q78(:,:)
       real*8,allocatable::S486(:,:,:,:)
       real*8,allocatable::S490(:,:,:,:)
       real*8,allocatable::S491(:,:,:,:)
       real*8,allocatable::S494(:,:,:,:)
       real*8,allocatable::S495(:,:,:,:)
       real*8,allocatable::S492(:,:,:,:)
       real*8,allocatable::S519(:,:,:,:)
       real*8,allocatable::S520(:,:,:,:)
       real*8,allocatable::S522(:,:,:,:)
       real*8,allocatable::S523(:,:,:,:)
       real*8,allocatable::S493(:,:,:,:)
       real*8,allocatable::S521(:,:,:,:)
       real*8,allocatable::S503(:,:,:,:)
       real*8,allocatable::S507(:,:,:,:)
       real*8,allocatable::S504(:,:,:,:)
       real*8,allocatable::Q79(:,:)
       real*8,allocatable::S508(:,:,:,:)
       real*8,allocatable::S500(:,:,:,:)
       real*8,allocatable::S506(:,:,:,:)
       real*8,allocatable::S404(:,:,:,:)
       real*8,allocatable::S405(:,:,:,:)
       real*8,allocatable::S406(:,:,:,:)
       real*8,allocatable::S407(:,:,:,:)
       real*8,allocatable::S496(:,:,:,:)
       real*8,allocatable::S497(:,:,:,:)
       real*8,allocatable::S414(:,:,:,:)
       real*8,allocatable::S479(:,:,:,:)
       real*8,allocatable::S415(:,:,:,:)
       real*8,allocatable::S411(:,:,:,:)
       real*8,allocatable::S477(:,:,:,:)
       real*8,allocatable::S412(:,:,:,:)
       real*8,allocatable::S498(:,:,:,:)
       real*8,allocatable::S499(:,:,:,:)
       real*8,allocatable::S501(:,:,:,:)
       real*8,allocatable::S502(:,:,:,:)
       real*8,allocatable::S505(:,:,:,:)
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
       real*8,allocatable::X24(:,:)
       real*8,allocatable::Z24(:,:,:,:,:,:)
       real*8,allocatable::X25(:,:)
       real*8,allocatable::Z25(:,:,:,:,:,:)
       real*8,allocatable::X26(:,:)
       real*8,allocatable::Z26(:,:,:,:,:,:)
       real*8,allocatable::X27(:,:)
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
       real*8,allocatable::Z78(:,:,:,:,:,:)
       real*8,allocatable::Z79(:,:,:,:,:,:)
       real*8,allocatable::Z81(:,:,:,:,:,:)
       real*8,allocatable::Z82(:,:,:,:,:,:)
       real*8,allocatable::X63(:,:,:,:)
       real*8,allocatable::Z88(:,:,:,:,:,:)
       real*8,allocatable::X64(:,:,:,:)
       real*8,allocatable::Z90(:,:,:,:,:,:)
       real*8,allocatable::X65(:,:,:,:)
       real*8,allocatable::Z89(:,:,:,:,:,:)
       real*8,allocatable::X66(:,:,:,:)
       real*8,allocatable::Z91(:,:,:,:,:,:)
       real*8,allocatable::X67(:,:,:,:)
       real*8,allocatable::Z92(:,:,:,:,:,:)
       real*8,allocatable::X68(:,:,:,:)
       real*8,allocatable::Z93(:,:,:,:,:,:)
       real*8,allocatable::X69(:,:,:,:)
       real*8,allocatable::Z94(:,:,:,:,:,:)
       real*8,allocatable::X70(:,:,:,:)
       real*8,allocatable::Z95(:,:,:,:,:,:)
       real*8,allocatable::X71(:,:)
       real*8,allocatable::Z96(:,:,:,:,:,:)
       real*8,allocatable::X72(:,:)
       real*8,allocatable::Z97(:,:,:,:,:,:)
       real*8,allocatable::X73(:,:)
       real*8,allocatable::Z102(:,:,:,:,:,:)
       real*8,allocatable::X74(:,:)
       real*8,allocatable::Z108(:,:,:,:,:,:)
       real*8,allocatable::X75(:,:)
       real*8,allocatable::Z103(:,:,:,:,:,:)
       real*8,allocatable::X76(:,:)
       real*8,allocatable::Z109(:,:,:,:,:,:)
       real*8,allocatable::Z110(:,:,:,:,:,:)
       real*8,allocatable::Z111(:,:,:,:,:,:)
       real*8,allocatable::Z112(:,:,:,:,:,:)
       real*8,allocatable::Z113(:,:,:,:,:,:)
       real*8,allocatable::X77(:,:,:,:)
       real*8,allocatable::Z124(:,:,:,:,:,:)
       real*8,allocatable::X78(:,:,:,:)
       real*8,allocatable::Z125(:,:,:,:,:,:)
       real*8,allocatable::Z176(:,:,:,:,:,:)
       real*8,allocatable::Z177(:,:,:,:,:,:)
       real*8,allocatable::Z178(:,:,:,:,:,:)
       real*8,allocatable::X79(:,:,:,:)
       real*8,allocatable::Z210(:,:,:,:,:,:)
       real*8,allocatable::X80(:,:,:,:)
       real*8,allocatable::Z211(:,:,:,:,:,:)
       real*8,allocatable::Z213(:,:,:,:,:,:)
       real*8,allocatable::Z214(:,:,:,:,:,:)
       real*8,allocatable::X81(:,:,:,:)
       real*8,allocatable::Z215(:,:,:,:,:,:)
       real*8,allocatable::X82(:,:,:,:)
       real*8,allocatable::Z216(:,:,:,:,:,:)
       real*8,allocatable::Z330(:,:,:,:,:,:)
       real*8,allocatable::Z331(:,:,:,:,:,:)
       real*8,allocatable::Z332(:,:,:,:,:,:)
       real*8,allocatable::Z333(:,:,:,:,:,:)
       real*8,allocatable::Z342(:,:,:,:,:,:)
       real*8,allocatable::Z343(:,:,:,:,:,:)
       real*8,allocatable::Z344(:,:,:,:,:,:)
       real*8,allocatable::Z345(:,:,:,:,:,:)
C
       allocate(V3C(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       V3C=0.0d0
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,N0-N0/),'2431',VBHPPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S1(M1+1:N1,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,N2+1:M2,N0+1:M1,M1+1:N1))
       X1=0.0d0
       call sum_stripe(4,shape(X1),size(X1),'4123',1.000,X1,
     & S1)
C
       allocate(X2(N0+1:N1,N2+1:M2,N0+1:M1,M1+1:N1))
       X2=0.0d0
       call sum_stripe(4,shape(X2),size(X2),'4123',1.000,X2,
     & S1)
       deallocate(S1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N0-N0/),'4231',VBHPPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S2(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N1+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       X3=0.0d0
       call sum_stripe(4,shape(X3),size(X3),'3124',-1.000,
     & X3,S2)
C
       allocate(X4(N1+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       X4=0.0d0
       call sum_stripe(4,shape(X4),size(X4),'3124',-1.000,
     & X4,S2)
       deallocate(S2)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHH),size(VBHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,M1-N0/),'4312',VBHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S3(N1+1:M2,N0+1:N2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:N2,N1+1:M2,N0+1:M1,M1+1:N1))
       X8=0.0d0
       call sum_stripe(4,shape(X8),size(X8),'2134',-1.000,
     & X8,S3)
       deallocate(S3)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPHP),size(VBHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,M1-N0/),'4132',VBHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S4(N1+1:M2,N2+1:N3,N2+1:M2,M1+1:N1))
       I1=K7*K0*K4
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       X9=0.0d0
       call sum_stripe(4,shape(X9),size(X9),'3124',-1.000,
     & X9,S4)
C
       allocate(X10(N2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       X10=0.0d0
       call sum_stripe(4,shape(X10),size(X10),'3124',-1.000,
     & X10,S4)
       deallocate(S4)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBPHPH),size(VBPHPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N0-N0/),'2341',VBPHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S5(M1+1:N1,N0+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'4123',1.000,X8,
     & S5)
       deallocate(S5)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBAPPP),size(VBAPPP),shape(D1),
     & size(D1),(/N1-N1,N2-N2,N2-N2,N1-N1/),'2134',VBAPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S6(M1+1:N1,N2+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0*K4
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'4123',1.000,X9,
     & S6)
C
       call sum_stripe(4,shape(X10),size(X10),'4123',1.000,
     & X10,S6)
       deallocate(S6)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,M1-N0/),'12',FAHP,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(Q1(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q1)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X19(M1+1:N1,M1+1:N1))
       X19=0.0d0
       call sum_stripe(2,shape(X19),size(X19),'21',1.000,
     & X19,Q1)
       deallocate(Q1)
C
       allocate(B1(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,M2-N1/),'21',FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q2(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q2)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X20(M2+1:N3,N1+1:M2))
       X20=0.0d0
       call sum_stripe(2,shape(X20),size(X20),'21',-1.000,
     & X20,Q2)
       deallocate(Q2)
C
       allocate(B1(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,N1-N1/),'21',FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q3(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q3)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X21(N1+1:M2,N1+1:M2))
       X21=0.0d0
       call sum_stripe(2,shape(X21),size(X21),'21',-1.000,
     & X21,Q3)
       deallocate(Q3)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S7(N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S7),size(S7),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,M1-M1/),'2314',S7,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z78(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z78)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z78)
       deallocate(Z78)
       deallocate(S7)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S8(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S8),size(S8),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N1-N1,M1-M1/),'2314',S8,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z79(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z79)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z79)
       deallocate(Z79)
       deallocate(S8)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M1-N0/),'3142',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q4(M1+1:N1,M1+1:N1))
       I1=K7*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       X19=X19+Q4
       deallocate(Q4)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N1,N1-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S9(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S9),size(S9),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,M1-M1/),'2341',S9,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z81(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z81)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z81)
       deallocate(Z81)
       deallocate(S9)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N1-N1,N1-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S10(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S10),size(S10),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N1-N1,M1-M1/),'2341',S10,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z82(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z82)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z82)
       deallocate(Z82)
       deallocate(S10)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q5(M2+1:N3,N1+1:M2))
       I1=K9*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       X20=X20-Q5
       deallocate(Q5)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q6(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       X21=X21-Q6
       deallocate(Q6)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N0-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S11(M1+1:N1,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X30(M1+1:N2,N0+1:M1,N0+1:M1,M1+1:N1))
       X30=0.0d0
       call sum_stripe(4,shape(X30),size(X30),'4123',1.000,
     & X30,S11)
       deallocate(S11)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N0-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S12(M1+1:N1,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X31(N0+1:M1,M1+1:N1,N0+1:M1,M1+1:N1))
       X31=0.0d0
       call sum_stripe(4,shape(X31),size(X31),'4123',1.000,
     & X31,S12)
       deallocate(S12)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,N0-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S13(M1+1:N1,M1+1:N2,M1+1:N1,N0+1:M1))
       I1=K5*K7*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X32(M1+1:N2,M1+1:N1,N0+1:M1,M1+1:N1))
       X32=0.0d0
       call sum_stripe(4,shape(X32),size(X32),'4123',1.000,
     & X32,S13)
       deallocate(S13)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N2,N2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S14(M1+1:N1,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X63(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N1))
       X63=0.0d0
       call sum_stripe(4,shape(X63),size(X63),'4123',1.000,
     & X63,S14)
C
       allocate(X64(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N1))
       X64=0.0d0
       call sum_stripe(4,shape(X64),size(X64),'4123',1.000,
     & X64,S14)
       deallocate(S14)
C
       allocate(D1(N1+1:N3,M1+1:N1,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,N2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S15(M1+1:N1,M1+1:N1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X65(M1+1:N1,N2+1:M2,N2+1:M2,M1+1:N1))
       X65=0.0d0
       call sum_stripe(4,shape(X65),size(X65),'4123',1.000,
     & X65,S15)
C
       allocate(X66(M1+1:N1,N2+1:M2,N2+1:M2,M1+1:N1))
       X66=0.0d0
       call sum_stripe(4,shape(X66),size(X66),'4123',1.000,
     & X66,S15)
       deallocate(S15)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N0-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S16(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X67(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X67=0.0d0
       call sum_stripe(4,shape(X67),size(X67),'3124',1.000,
     & X67,S16)
       deallocate(S16)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N0-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S17(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X68(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       X68=0.0d0
       call sum_stripe(4,shape(X68),size(X68),'3124',1.000,
     & X68,S17)
       deallocate(S17)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S18(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X69(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X69=0.0d0
       call sum_stripe(4,shape(X69),size(X69),'3124',1.000,
     & X69,S18)
       deallocate(S18)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N0-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S19(N1+1:M2,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X70(M1+1:N2,N1+1:M2,N1+1:M2,N0+1:M1))
       X70=0.0d0
       call sum_stripe(4,shape(X70),size(X70),'3124',1.000,
     & X70,S19)
       deallocate(S19)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q7(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X71(N0+1:M1,N0+1:M1))
       X71=0.0d0
       X71=X71+Q7
       deallocate(Q7)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q8(M1+1:N2,N0+1:M1))
       I1=K5*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X72(M1+1:N2,N0+1:M1))
       X72=0.0d0
       X72=X72+Q8
       deallocate(Q8)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S20(N1+1:M2,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X41(M2+1:N3,M2+1:N3,N2+1:M2,N1+1:M2))
       X41=0.0d0
       call sum_stripe(4,shape(X41),size(X41),'4123',-1.000,
     & X41,S20)
C
       allocate(X45(M2+1:N3,M2+1:N3,N2+1:M2,N1+1:M2))
       X45=0.0d0
       call sum_stripe(4,shape(X45),size(X45),'4123',-1.000,
     & X45,S20)
       deallocate(S20)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S22(N1+1:M2,M2+1:N3,N1+1:M2,N2+1:M2))
       I1=K0*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X43(M2+1:N3,N1+1:M2,N2+1:M2,N1+1:M2))
       X43=0.0d0
       call sum_stripe(4,shape(X43),size(X43),'4123',-1.000,
     & X43,S22)
C
       allocate(X47(M2+1:N3,N1+1:M2,N2+1:M2,N1+1:M2))
       X47=0.0d0
       call sum_stripe(4,shape(X47),size(X47),'4123',-1.000,
     & X47,S22)
       deallocate(S22)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q9(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X73(M2+1:N3,N2+1:M2))
       X73=0.0d0
       X73=X73+Q9
C
       allocate(X74(M2+1:N3,N2+1:M2))
       X74=0.0d0
       X74=X74+Q9
       deallocate(Q9)
C
       allocate(D1(N0+1:N1,N2+1:M2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S21(N1+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K0
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X42(N2+1:M2,M2+1:N3,N2+1:M2,N1+1:M2))
       X42=0.0d0
       call sum_stripe(4,shape(X42),size(X42),'4123',-1.000,
     & X42,S21)
C
       allocate(X46(N2+1:M2,M2+1:N3,N2+1:M2,N1+1:M2))
       X46=0.0d0
       call sum_stripe(4,shape(X46),size(X46),'4123',-1.000,
     & X46,S21)
       deallocate(S21)
C
       allocate(D1(N0+1:N1,N2+1:M2,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N2-N2/),'4123',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S23(N1+1:M2,N2+1:M2,N1+1:M2,N2+1:M2))
       I1=K0*K9*K0
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X44(N2+1:M2,N1+1:M2,N2+1:M2,N1+1:M2))
       X44=0.0d0
       call sum_stripe(4,shape(X44),size(X44),'4123',-1.000,
     & X44,S23)
C
       allocate(X48(N2+1:M2,N1+1:M2,N2+1:M2,N1+1:M2))
       X48=0.0d0
       call sum_stripe(4,shape(X48),size(X48),'4123',-1.000,
     & X48,S23)
       deallocate(S23)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q10(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X75(N2+1:M2,N2+1:M2))
       X75=0.0d0
       X75=X75+Q10
C
       allocate(X76(N2+1:M2,N2+1:M2))
       X76=0.0d0
       X76=X76+Q10
       deallocate(Q10)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S24(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S24),size(S24),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,M1-M1/),'2314',S24,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z110(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z110)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z110)
       deallocate(Z110)
       deallocate(S24)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S25(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S25),size(S25),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,M1-M1/),'2314',S25,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z111(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z111)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z111)
       deallocate(Z111)
       deallocate(S25)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S26(M1+1:N1,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S26),size(S26),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,M1-M1/),'2341',S26,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z112(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z112)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z112)
       deallocate(Z112)
       deallocate(S26)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S27(M1+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S27),size(S27),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,M1-M1/),'2341',S27,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z113(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z113)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z113)
       deallocate(Z113)
       deallocate(S27)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHH),size(VBHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,M1-N0/),'3412',VBHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S28(N2+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S28)
       deallocate(S28)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHH),size(VBHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,M1-N0/),'3412',VBHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S29(N2+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S29)
       deallocate(S29)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPHP),size(VBHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,M1-N0/),'1432',VBHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S30(N0+1:M1,N0+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K1
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'3124',1.000,X1,
     & S30)
       deallocate(S30)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPHP),size(VBHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,M1-N0/),'1432',VBHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S31(N0+1:M1,N0+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K1
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'3124',1.000,X2,
     & S31)
       deallocate(S31)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBPHPH),size(VBPHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3241',VBPHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S32(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',-1.000,
     & X3,S32)
       deallocate(S32)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBPHPH),size(VBPHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3241',VBPHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S33(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',-1.000,
     & X4,S33)
       deallocate(S33)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBAPPP),size(VBAPPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N2-N2,N1-N1/),'1234',VBAPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S34(N0+1:M1,N1+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0*K3
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'4123',1.000,X3,
     & S34)
       deallocate(S34)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBAPPP),size(VBAPPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N2-N2,N1-N1/),'1234',VBAPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S35(N0+1:M1,N1+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0*K3
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'4123',1.000,X4,
     & S35)
       deallocate(S35)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VCHHHH),size(VCHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,N0-N0/),'3412',VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S36(N2+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       X5=0.0d0
       call sum_stripe(4,shape(X5),size(X5),'2134',-1.000,
     & X5,S36)
       deallocate(S36)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VCHHHH),size(VCHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,N0-N0/),'3412',VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S37(N2+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       X6=0.0d0
       call sum_stripe(4,shape(X6),size(X6),'2134',-1.000,
     & X6,S37)
       deallocate(S37)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N0-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S38(N0+1:M1,N0+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X77(N0+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       X77=0.0d0
       call sum_stripe(4,shape(X77),size(X77),'3124',1.000,
     & X77,S38)
       deallocate(S38)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N0-N0/),'1432',VCHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S39(N0+1:M1,N0+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X78(N0+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       X78=0.0d0
       call sum_stripe(4,shape(X78),size(X78),'3124',1.000,
     & X78,S39)
       deallocate(S39)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S40(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       X7=0.0d0
       call sum_stripe(4,shape(X7),size(X7),'3124',-1.000,
     & X7,S40)
       deallocate(S40)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'4132',VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S41(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',1.000,X7,
     & S41)
       deallocate(S41)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCAPPP),size(VCAPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N2-N2,N2-N2/),'1234',VCAPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S42(N0+1:M1,N2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K4
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'4123',-1.000,
     & X7,S42)
       deallocate(S42)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,M1-N0/),'3124',VBHPPH,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S43(N0+1:M1,N0+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'3124',1.000,X8,
     & S43)
       deallocate(S43)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'1324',VBHPPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S44(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',-1.000,
     & X9,S44)
       deallocate(S44)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'1324',VBHPPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S45(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',-1.000,
     & X10,S45)
       deallocate(S45)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S46(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X11=0.0d0
       call sum_stripe(4,shape(X11),size(X11),'3124',-1.000,
     & X11,S46)
       deallocate(S46)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S47(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X12=0.0d0
       call sum_stripe(4,shape(X12),size(X12),'3124',-1.000,
     & X12,S47)
       deallocate(S47)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S48(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:M1,N1+1:M2,N2+1:M2,N0+1:M1))
       X13=0.0d0
       call sum_stripe(4,shape(X13),size(X13),'3124',-1.000,
     & X13,S48)
       deallocate(S48)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S49(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(M1+1:N1,N1+1:M2,N2+1:M2,N0+1:M1))
       X14=0.0d0
       call sum_stripe(4,shape(X14),size(X14),'3124',-1.000,
     & X14,S49)
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
       allocate(X15(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X15=0.0d0
       call sum_stripe(4,shape(X15),size(X15),'3124',-1.000,
     & X15,S50)
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
       allocate(X16(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X16=0.0d0
       call sum_stripe(4,shape(X16),size(X16),'3124',-1.000,
     & X16,S51)
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
       allocate(X17(N0+1:M1,N1+1:M2,N2+1:M2,N0+1:M1))
       X17=0.0d0
       call sum_stripe(4,shape(X17),size(X17),'3124',-1.000,
     & X17,S52)
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
       allocate(X18(M1+1:N1,N1+1:M2,N2+1:M2,N0+1:M1))
       X18=0.0d0
       call sum_stripe(4,shape(X18),size(X18),'3124',-1.000,
     & X18,S53)
       deallocate(S53)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S54(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'4123',1.000,
     & X11,S54)
       deallocate(S54)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S55(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'4123',1.000,
     & X12,S55)
       deallocate(S55)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S56(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X13),size(X13),'4123',1.000,
     & X13,S56)
       deallocate(S56)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S57(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X14),size(X14),'4123',1.000,
     & X14,S57)
       deallocate(S57)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S58(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X15),size(X15),'4123',1.000,
     & X15,S58)
       deallocate(S58)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S59(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X16),size(X16),'4123',1.000,
     & X16,S59)
       deallocate(S59)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S60(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X17),size(X17),'4123',1.000,
     & X17,S60)
       deallocate(S60)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S61(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X18),size(X18),'4123',1.000,
     & X18,S61)
       deallocate(S61)
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
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q13(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q13)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X24(M2+1:N3,N2+1:M2))
       X24=0.0d0
       call sum_stripe(2,shape(X24),size(X24),'21',-1.000,
     & X24,Q13)
       deallocate(Q13)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,N2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q14(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q14)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X25(N2+1:M2,N2+1:M2))
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
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,M1-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S62(N0+1:M1,M1+1:N2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S62)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X30),size(X30),'3124',1.000,
     & X30,S62)
       deallocate(S62)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M1-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S63(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X31),size(X31),'3124',1.000,
     & X31,S63)
       deallocate(S63)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,M1-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S64(N0+1:M1,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X32),size(X32),'3124',1.000,
     & X32,S64)
       deallocate(S64)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S65(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X33(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N1))
       X33=0.0d0
       call sum_stripe(4,shape(X33),size(X33),'3124',-1.000,
     & X33,S65)
       deallocate(S65)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,M1,N1,X33,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'612345',t3C4,F2)
       allocate(Z33(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K5*K5*K9*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X33,F2,Z33)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234516',
     & -1.000,V3C,Z33)
       deallocate(Z33)
       deallocate(X33)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S66(N2+1:M2,M1+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S66)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X34(M1+1:N1,N2+1:M2,N2+1:M2,M1+1:N1))
       X34=0.0d0
       call sum_stripe(4,shape(X34),size(X34),'3124',-1.000,
     & X34,S66)
       deallocate(S66)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,N2,M2,N2,M2,M1,N1,X34,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'612345',t3C4,F2)
       allocate(Z34(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K5*K5*K9*K0
       I3=K0*K7
       call EGEMM(I1,I2,I3,X34,F2,Z34)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234516',
     & -1.000,V3C,Z34)
       deallocate(Z34)
       deallocate(X34)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S67(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X35(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N1))
       X35=0.0d0
       call sum_stripe(4,shape(X35),size(X35),'3124',-1.000,
     & X35,S67)
       deallocate(S67)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,M1,N1,X35,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'612345',t3C4,F2)
       allocate(Z35(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K5*K5*K9*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X35,F2,Z35)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134526',
     & 1.000,V3C,Z35)
       deallocate(Z35)
       deallocate(X35)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'3412',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S68(N2+1:M2,M1+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S68)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X36(M1+1:N1,N2+1:M2,N2+1:M2,M1+1:N1))
       X36=0.0d0
       call sum_stripe(4,shape(X36),size(X36),'3124',-1.000,
     & X36,S68)
       deallocate(S68)
C
       call sumx_sorted2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,N2,M2,N2,M2,M1,N1,X36,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'612345',t3C4,F2)
       allocate(Z36(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K5*K5*K9*K0
       I3=K0*K7
       call EGEMM(I1,I2,I3,X36,F2,Z36)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134526',
     & 1.000,V3C,Z36)
       deallocate(Z36)
       deallocate(X36)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q17(M1+1:N1,M1+1:N1))
       I1=K7*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q17)
       deallocate(D1)
       deallocate(B2)
C
       X19=X19+Q17
       deallocate(Q17)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S69(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X37(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X37=0.0d0
       call sum_stripe(4,shape(X37),size(X37),'4123',1.000,
     & X37,S69)
       deallocate(S69)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,N1,M2,N0,M1,X37,VBPHPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N2-N2,N2-N2,N0-N0,M1-M1/),'431256',t3C4,F2)
       allocate(Z37(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X37,F2,Z37)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124635',
     & 1.000,V3C,Z37)
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & -1.000,V3C,Z37)
       deallocate(Z37)
       deallocate(X37)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S70(N0+1:M1,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S70)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X38(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       X38=0.0d0
       call sum_stripe(4,shape(X38),size(X38),'4123',1.000,
     & X38,S70)
       deallocate(S70)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,N0,M1,X38,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N2-N2,N2-N2,N0-N0,M1-N0/),'531246',t3C1,F2)
       allocate(Z38(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X38,F2,Z38)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124635',
     & -1.000,V3C,Z38)
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & 1.000,V3C,Z38)
       deallocate(Z38)
       deallocate(X38)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S71(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X39(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X39=0.0d0
       call sum_stripe(4,shape(X39),size(X39),'4123',1.000,
     & X39,S71)
       deallocate(S71)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,M1,N1,M2,N1,M2,N0,M1,X39,VBPHPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N2-N2,N2-N2,N0-N0,M1-M1/),'431256',t3C4,F2)
       allocate(Z39(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,X39,F2,Z39)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124635',
     & 1.000,V3C,Z39)
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & -1.000,V3C,Z39)
       deallocate(Z39)
       deallocate(X39)
C
       allocate(D1(N2+1:N3,M1+1:N2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N1-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S72(N0+1:M1,M1+1:N2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S72)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X40(M1+1:N2,N1+1:M2,N1+1:M2,N0+1:M1))
       X40=0.0d0
       call sum_stripe(4,shape(X40),size(X40),'4123',1.000,
     & X40,S72)
       deallocate(S72)
C
       call sumx_sorted4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,N1,M2,N1,M2,N0,M1,X40,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N2-N2,N2-N2,N0-N0,M1-N0/),'531246',t3C1,F2)
       allocate(Z40(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K0*K0
       I3=K9*K8
       call EGEMM(I1,I2,I3,X40,F2,Z40)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124635',
     & -1.000,V3C,Z40)
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & 1.000,V3C,Z40)
       deallocate(Z40)
       deallocate(X40)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S73(N2+1:M2,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X41),size(X41),'3124',-1.000,
     & X41,S73)
       deallocate(S73)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S74(N2+1:M2,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S74)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X42),size(X42),'3124',-1.000,
     & X42,S74)
       deallocate(S74)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S75(N2+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X43),size(X43),'3124',-1.000,
     & X43,S75)
       deallocate(S75)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S76(N2+1:M2,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S76)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X44),size(X44),'3124',-1.000,
     & X44,S76)
       deallocate(S76)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S77(N2+1:M2,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X45),size(X45),'3124',-1.000,
     & X45,S77)
       deallocate(S77)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S78(N2+1:M2,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S78)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X46),size(X46),'3124',-1.000,
     & X46,S78)
       deallocate(S78)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S79(N2+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X47),size(X47),'3124',-1.000,
     & X47,S79)
       deallocate(S79)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S80(N2+1:M2,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S80)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X48),size(X48),'3124',-1.000,
     & X48,S80)
       deallocate(S80)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q18(M2+1:N3,N1+1:M2))
       I1=K9*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q18)
       deallocate(D1)
       deallocate(B2)
C
       X20=X20+Q18
       deallocate(Q18)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q19(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q19)
       deallocate(D1)
       deallocate(B2)
C
       X21=X21+Q19
       deallocate(Q19)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N0-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S81(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S81)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S81),size(S81),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S81,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2,N1-N1,M1-M1/),'451236',t3C4,F2)
       allocate(Z176(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K0*K0
       I3=K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z176)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123645',
     & 0.500,V3C,Z176)
       call sum_stripe(6,shape(V3C),size(V3C),'123654',
     & -0.500,V3C,Z176)
       deallocate(Z176)
       deallocate(S81)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N0-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S82(N0+1:M1,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S82)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S82),size(S82),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N0-N0,N0-N0/),'2314',S82,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(Z177(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K0*K0
       I3=K8*K5
       call EGEMM(I1,I2,I3,D1,F2,Z177)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123645',
     & 1.000,V3C,Z177)
       call sum_stripe(6,shape(V3C),size(V3C),'123654',
     & -1.000,V3C,Z177)
       deallocate(Z177)
       deallocate(S82)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,N0-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S83(N0+1:M1,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S83),size(S83),shape(D1),size(D1),
     & (/M1-M1,M1-M1,N0-N0,N0-N0/),'2314',S83,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(Z178(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K0*K0
       I3=K8*K8
       call EGEMM(I1,I2,I3,D1,F2,Z178)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123645',
     & 0.500,V3C,Z178)
       call sum_stripe(6,shape(V3C),size(V3C),'123654',
     & -0.500,V3C,Z178)
       deallocate(Z178)
       deallocate(S83)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S84(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S84)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X52(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X52=0.0d0
       call sum_stripe(4,shape(X52),size(X52),'3124',-1.000,
     & X52,S84)
       deallocate(S84)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S85(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S85)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X53(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       X53=0.0d0
       call sum_stripe(4,shape(X53),size(X53),'3124',-1.000,
     & X53,S85)
       deallocate(S85)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S86(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S86)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X54(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X54=0.0d0
       call sum_stripe(4,shape(X54),size(X54),'3124',-1.000,
     & X54,S86)
       deallocate(S86)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S87(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S87)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X55(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       X55=0.0d0
       call sum_stripe(4,shape(X55),size(X55),'3124',-1.000,
     & X55,S87)
       deallocate(S87)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S88(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S88)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X56(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X56=0.0d0
       call sum_stripe(4,shape(X56),size(X56),'3124',-1.000,
     & X56,S88)
       deallocate(S88)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S89(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S89)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X57(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       X57=0.0d0
       call sum_stripe(4,shape(X57),size(X57),'3124',-1.000,
     & X57,S89)
       deallocate(S89)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S90(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X58(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X58=0.0d0
       call sum_stripe(4,shape(X58),size(X58),'3124',-1.000,
     & X58,S90)
       deallocate(S90)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S91(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X59(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       X59=0.0d0
       call sum_stripe(4,shape(X59),size(X59),'3124',-1.000,
     & X59,S91)
       deallocate(S91)
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
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S92(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S92)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X52),size(X52),'4123',-1.000,
     & X52,S92)
       deallocate(S92)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S93(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X53),size(X53),'4123',-1.000,
     & X53,S93)
       deallocate(S93)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S94(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S94)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X54),size(X54),'4123',-1.000,
     & X54,S94)
       deallocate(S94)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S95(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X55),size(X55),'4123',-1.000,
     & X55,S95)
       deallocate(S95)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S96(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S96)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X56),size(X56),'4123',-1.000,
     & X56,S96)
       deallocate(S96)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S97(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X57),size(X57),'4123',-1.000,
     & X57,S97)
       deallocate(S97)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S98(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S98)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X58),size(X58),'4123',-1.000,
     & X58,S98)
       deallocate(S98)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S99(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X59),size(X59),'4123',-1.000,
     & X59,S99)
       deallocate(S99)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S100(N2+1:M2,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S100)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X60(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       X60=0.0d0
       call sum_stripe(4,shape(X60),size(X60),'4123',-1.000,
     & X60,S100)
       deallocate(S100)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S101(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X61(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       X61=0.0d0
       call sum_stripe(4,shape(X61),size(X61),'4123',-1.000,
     & X61,S101)
       deallocate(S101)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S102(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S102)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X62(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       X62=0.0d0
       call sum_stripe(4,shape(X62),size(X62),'4123',-1.000,
     & X62,S102)
       deallocate(S102)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q22(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q22)
       deallocate(D1)
       deallocate(B2)
C
       X24=X24-Q22
       deallocate(Q22)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q23(N2+1:M2,N2+1:M2))
       I1=K0*K0
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
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S103(N2+1:M2,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X60),size(X60),'3124',1.000,
     & X60,S103)
       deallocate(S103)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S104(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S104)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X61),size(X61),'3124',1.000,
     & X61,S104)
       deallocate(S104)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S105(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X62),size(X62),'3124',1.000,
     & X62,S105)
       deallocate(S105)
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
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S106(N1+1:M2,M1+1:N1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S106)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2413',1.000,X8,
     & S106)
       deallocate(S106)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S107(N1+1:M2,M1+1:N1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S107)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'3412',1.000,X9,
     & S107)
C
       call sum_stripe(4,shape(X10),size(X10),'3412',1.000,
     & X10,S107)
       deallocate(S107)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S108(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S108)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X79(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       X79=0.0d0
       call sum_stripe(4,shape(X79),size(X79),'3412',1.000,
     & X79,S108)
       deallocate(S108)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S109(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S109)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X80(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X80=0.0d0
       call sum_stripe(4,shape(X80),size(X80),'3412',1.000,
     & X80,S109)
       deallocate(S109)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Q26(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q26)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X19),size(X19),'21',0.500,
     & X19,Q26)
       deallocate(Q26)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N1/),'3421',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(Q27(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q27)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(M2+1:N3,N1+1:M2))
       call reorder_shift(2,shape(Q27),size(Q27),shape(B1),size(B1),
     & (/M2-M2,N1-N1/),'21',Q27,B1)
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M2-N1,N2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'312456',t3C4,F2)
       allocate(Z213(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K5*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,B1,F2,Z213)
       deallocate(B1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124563',
     & 0.500,V3C,Z213)
       deallocate(Z213)
       deallocate(Q27)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(Q28(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q28)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N1+1:M2,N1+1:M2))
       call reorder_shift(2,shape(Q28),size(Q28),shape(B1),size(B1),
     & (/N1-N1,N1-N1/),'21',Q28,B1)
       allocate(F2(N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N1-N1,N2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'312456',t3C4,F2)
       allocate(Z214(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K5*K0*K0
       I3=K9
       call EGEMM(I1,I2,I3,B1,F2,Z214)
       deallocate(B1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124563',
     & 0.500,V3C,Z214)
       deallocate(Z214)
       deallocate(Q28)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S110(N1+1:M2,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S110)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X81(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       X81=0.0d0
       call sum_stripe(4,shape(X81),size(X81),'3412',1.000,
     & X81,S110)
       deallocate(S110)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S111(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S111)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X82(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       X82=0.0d0
       call sum_stripe(4,shape(X82),size(X82),'3412',1.000,
     & X82,S111)
       deallocate(S111)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,N0-N0,M1-N0/),'2134',t2B,D2)
       allocate(S112(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S112)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S112)
       deallocate(S112)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,N0-N0,M1-N0/),'2134',t2B,D2)
       allocate(S113(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S113)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S113)
       deallocate(S113)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'3142',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S114(N2+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S114)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2314',1.000,X2,
     & S114)
       deallocate(S114)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'3142',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S115(N2+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S115)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S115)
       deallocate(S115)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S116(N2+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S116)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2413',-1.000,
     & X4,S116)
       deallocate(S116)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S117(N2+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S117)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2413',-1.000,
     & X3,S117)
       deallocate(S117)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S118(N2+1:M2,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S118)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2314',1.000,X4,
     & S118)
       deallocate(S118)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'3421',VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S119(N2+1:M2,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S119)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',1.000,X3,
     & S119)
       deallocate(S119)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S120(N2+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S120)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X78),size(X78),'2314',1.000,
     & X78,S120)
       deallocate(S120)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'3241',VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S121(N2+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S121)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2413',-1.000,
     & X1,S121)
       deallocate(S121)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S122(N2+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S122)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X77),size(X77),'2314',1.000,
     & X77,S122)
       deallocate(S122)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'3241',VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S123(N2+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S123)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2413',-1.000,
     & X2,S123)
       deallocate(S123)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N2-N2/),'4123',VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S124(N1+1:M2,N0+1:M1,N1+1:N3,N2+1:M2))
       I1=K0*K3
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S124)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'3412',-1.000,
     & X3,S124)
       deallocate(S124)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S125(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S125)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'3412',1.000,X7,
     & S125)
       deallocate(S125)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,N2-N2/),'1243',VBHPPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,M1-N0/),'1234',t2B,D2)
       allocate(S126(N0+1:M1,M1+1:N1,N0+1:N1,N2+1:M2))
       I1=K0*K1
       I2=K7*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S126)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'3412',1.000,X1,
     & S126)
       deallocate(S126)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N2-N2/),'4123',VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S127(N1+1:M2,N0+1:M1,N1+1:N3,N2+1:M2))
       I1=K0*K3
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S127)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'3412',-1.000,
     & X4,S127)
       deallocate(S127)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S128(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S128)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2413',-1.000,
     & X7,S128)
       deallocate(S128)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,N2-N2/),'1243',VBHPPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,M1-N0/),'1234',t2B,D2)
       allocate(S129(N0+1:M1,M1+1:N1,N0+1:N1,N2+1:M2))
       I1=K0*K1
       I2=K7*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S129)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'3412',1.000,X2,
     & S129)
       deallocate(S129)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(D2(N2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,M1-N0/),'1234',t2B,D2)
       allocate(S130(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S130)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S130)
       deallocate(S130)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S131(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S131)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S131)
       deallocate(S131)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'12',FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S132(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S132)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S132)
       deallocate(S132)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S133(N2+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S133)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2314',1.000,X2,
     & S133)
       deallocate(S133)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S134(N2+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S134)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S134)
       deallocate(S134)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'4132',VBHHHP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S135(N1+1:M2,N0+1:M1,N0+1:N2,M1+1:N1))
       I1=K7*K2
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S135)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2314',-1.000,
     & X8,S135)
       deallocate(S135)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'3412',VBHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S136(N2+1:M2,N1+1:M2,N2+1:N3,M1+1:N1))
       I1=K7*K4
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S136)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2314',1.000,X9,
     & S136)
       deallocate(S136)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'3412',VBHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S137(N2+1:M2,N1+1:M2,N2+1:N3,M1+1:N1))
       I1=K7*K4
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S137)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2314',1.000,
     & X10,S137)
       deallocate(S137)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S138(N2+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S138)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2413',1.000,X4,
     & S138)
       deallocate(S138)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S139(N2+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S139)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2413',1.000,X3,
     & S139)
       deallocate(S139)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,N1-N1/),'1234',VBPHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,M1-N0/),'1234',t2B,D2)
       allocate(S140(N0+1:M1,M1+1:N1,N0+1:N2,N1+1:M2))
       I1=K9*K2
       I2=K7*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S140)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'3412',1.000,X8,
     & S140)
       deallocate(S140)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N1-N1/),'3214',VBPHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S141(N2+1:M2,M1+1:N1,N2+1:N3,N1+1:M2))
       I1=K9*K4
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S141)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2413',-1.000,
     & X9,S141)
       deallocate(S141)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N1-N1/),'3214',VBPHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S142(N2+1:M2,M1+1:N1,N2+1:N3,N1+1:M2))
       I1=K9*K4
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S142)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2413',-1.000,
     & X10,S142)
       deallocate(S142)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'3412',VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S143(N2+1:M2,N2+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S143)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2314',0.500,X7,
     & S143)
       deallocate(S143)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S144(N2+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S144)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X78),size(X78),'2314',1.000,
     & X78,S144)
       deallocate(S144)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S145(N2+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S145)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X77),size(X77),'2314',1.000,
     & X77,S145)
       deallocate(S145)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'4132',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S146(N1+1:M2,M1+1:N1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S146)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2413',-1.000,
     & X8,S146)
       deallocate(S146)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S147(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S147)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'3412',-1.000,
     & X7,S147)
       deallocate(S147)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S148(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S148)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2413',1.000,X7,
     & S148)
       deallocate(S148)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N2-N2/),'1243',VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S149(N0+1:M1,N0+1:M1,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K5*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S149)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'3412',0.500,X5,
     & S149)
       deallocate(S149)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4213',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S150(N1+1:M2,M1+1:N1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S150)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'3412',1.000,X9,
     & S150)
       deallocate(S150)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N2-N2/),'1243',VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S151(N0+1:M1,N0+1:M1,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K5*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S151)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'3412',0.500,X6,
     & S151)
       deallocate(S151)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4213',VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S152(N1+1:M2,M1+1:N1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S152)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'3412',1.000,
     & X10,S152)
       deallocate(S152)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,N2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S153(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S153)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S153)
       deallocate(S153)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N1,N2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S154(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S154)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',0.500,X4,
     & S154)
       deallocate(S154)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,N2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S155(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S155)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S155)
       deallocate(S155)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N1-N1,N2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S156(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S156)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',0.500,X4,
     & S156)
       deallocate(S156)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,N2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S157(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S157)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',1.000,X3,
     & S157)
       deallocate(S157)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N1,N2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S158(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S158)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',0.500,X3,
     & S158)
       deallocate(S158)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,N2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S159(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S159)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',1.000,X3,
     & S159)
       deallocate(S159)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N1-N1,N2-N2,N1-N1,N0-N0/),'562134',t3B1,F2)
       allocate(S160(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S160)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',0.500,X3,
     & S160)
       deallocate(S160)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,M2-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,M2-M2,N2-N2,N0-N0,M1-M1/),'523146',t3B3,F2)
       allocate(S161(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S161)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',0.500,X2,
     & S161)
       deallocate(S161)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,M2-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,M2-M2,N2-N2,N0-N0,M1-M1/),'523146',t3B3,F2)
       allocate(S162(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S162)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',0.500,X2,
     & S162)
       deallocate(S162)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N1-N1,N2-N2,N0-N0,M1-M1/),'523146',t3B1,F2)
       allocate(S163(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S163)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S163)
       deallocate(S163)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N1-N1,N2-N2,N0-N0,M1-M1/),'523146',t3B1,F2)
       allocate(S164(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S164)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S164)
       deallocate(S164)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N1-N1,N2-N2,N0-N0,M1-M1/),'523146',t3B1,F2)
       allocate(S165(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S165)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',0.500,X2,
     & S165)
       deallocate(S165)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N1-N1,N2-N2,N0-N0,M1-M1/),'523146',t3B1,F2)
       allocate(S166(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S166)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',0.500,X2,
     & S166)
       deallocate(S166)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S167(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S167)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'3412',1.000,
     & X11,S167)
       deallocate(S167)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S168(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S168)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'3412',1.000,
     & X12,S168)
       deallocate(S168)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S169(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S169)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'3412',1.000,
     & X13,S169)
       deallocate(S169)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S170(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S170)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'3412',1.000,
     & X14,S170)
       deallocate(S170)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,M2-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/N0-N0,M2-M2,M2-M2,N2-N2,N0-N0,M1-M1/),'523146',t3B3,F2)
       allocate(S171(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S171)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',0.500,X1,
     & S171)
       deallocate(S171)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,M2-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/M1-N0,M2-M2,M2-M2,N2-N2,N0-N0,M1-M1/),'523146',t3B3,F2)
       allocate(S172(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S172)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',0.500,X1,
     & S172)
       deallocate(S172)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N1-N1,N2-N2,N0-N0,M1-M1/),'523146',t3B1,F2)
       allocate(S173(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S173)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S173)
       deallocate(S173)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N1-N1,N2-N2,N0-N0,M1-M1/),'523146',t3B1,F2)
       allocate(S174(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S174)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S174)
       deallocate(S174)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N1-N1,N2-N2,N0-N0,M1-M1/),'523146',t3B1,F2)
       allocate(S175(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S175)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',0.500,X1,
     & S175)
       deallocate(S175)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N1-N1,N2-N2,N0-N0,M1-M1/),'523146',t3B1,F2)
       allocate(S176(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S176)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',0.500,X1,
     & S176)
       deallocate(S176)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S177(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S177)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X15),size(X15),'3412',1.000,
     & X15,S177)
       deallocate(S177)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S178(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S178)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X16),size(X16),'3412',1.000,
     & X16,S178)
       deallocate(S178)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S179(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S179)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X17),size(X17),'3412',1.000,
     & X17,S179)
       deallocate(S179)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S180(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S180)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X18),size(X18),'3412',1.000,
     & X18,S180)
       deallocate(S180)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(S181(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S181)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S181)
       deallocate(S181)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,N2-N2,N1-N1,N0-N0/),'461235',t3C4,F2)
       allocate(S182(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S182)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S182)
       deallocate(S182)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,M2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(S183(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S183)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S183)
       deallocate(S183)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(S184(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S184)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S184)
       deallocate(S184)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,N2-N2,N1-N1,N0-N0/),'461235',t3C4,F2)
       allocate(S185(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S185)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S185)
       deallocate(S185)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,N2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(S186(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S186)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S186)
       deallocate(S186)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(S187(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S187)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',1.000,X3,
     & S187)
       deallocate(S187)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,N2-N2,N1-N1,N0-N0/),'461235',t3C4,F2)
       allocate(S188(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S188)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S188)
       deallocate(S188)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,M2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(S189(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S189)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',1.000,X3,
     & S189)
       deallocate(S189)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(S190(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S190)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',1.000,X3,
     & S190)
       deallocate(S190)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,N2-N2,N1-N1,N0-N0/),'461235',t3C4,F2)
       allocate(S191(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S191)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S191)
       deallocate(S191)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,N2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(S192(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S192)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',1.000,X3,
     & S192)
       deallocate(S192)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-N1,N2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S193(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S193)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S193)
       deallocate(S193)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,N2-N2,N2-N2,N0-N0/),'463125',t3C4,F2)
       allocate(S194(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S194)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',-1.000,
     & X7,S194)
       deallocate(S194)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,M2-N1,N2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S195(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S195)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S195)
       deallocate(S195)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N1-N1,N2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S196(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S196)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S196)
       deallocate(S196)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,N2-N2,N2-N2,N0-N0/),'463125',t3C4,F2)
       allocate(S197(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S197)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',-1.000,
     & X7,S197)
       deallocate(S197)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,N1-N1,N2-N2,N2-N2,N0-N0/),'563124',t3C1,F2)
       allocate(S198(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S198)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S198)
       deallocate(S198)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,M1-N0,N0-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,M1-N0/),'1234',t2B,D2)
       allocate(S199(N0+1:M1,M1+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K7*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S199)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X30),size(X30),'3412',1.000,
     & X30,S199)
       deallocate(S199)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,M1-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,M1-N0/),'1234',t2B,D2)
       allocate(S200(N0+1:M1,M1+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K7*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S200)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X31),size(X31),'3412',1.000,
     & X31,S200)
       deallocate(S200)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,M1-N0,M1-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,M1-N0/),'1234',t2B,D2)
       allocate(S201(N0+1:M1,M1+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K7*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S201)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X32),size(X32),'3412',1.000,
     & X32,S201)
       deallocate(S201)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N2-N2,N0-N0,N0-N0/),'613245',t3C4,F2)
       allocate(S202(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S202)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S202)
       deallocate(S202)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N2-N2,N0-N0,N0-N0/),'613245',t3C4,F2)
       allocate(S203(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S203)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S203)
       deallocate(S203)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N2-N2,N0-N0,N0-N0/),'613245',t3C4,F2)
       allocate(S204(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S204)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S204)
       deallocate(S204)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N2-N2,N0-N0,N0-N0/),'613245',t3C4,F2)
       allocate(S205(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S205)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S205)
       deallocate(S205)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S206(N2+1:M2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S206)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X63),size(X63),'3412',-1.000,
     & X63,S206)
       deallocate(S206)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S207(N2+1:M2,M1+1:N1,M1+1:N1,N2+1:M2))
       I1=K0*K7
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S207)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X65),size(X65),'3412',-1.000,
     & X65,S207)
       deallocate(S207)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N2-N2,N0-N0,N0-N0/),'613245',t3C4,F2)
       allocate(S208(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S208)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S208)
       deallocate(S208)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N2-N2,N0-N0,N0-N0/),'613245',t3C4,F2)
       allocate(S209(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S209)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S209)
       deallocate(S209)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N2-N2,N0-N0,N0-N0/),'613245',t3C4,F2)
       allocate(S210(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S210)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S210)
       deallocate(S210)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N2-N2,N0-N0,N0-N0/),'613245',t3C4,F2)
       allocate(S211(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S211)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S211)
       deallocate(S211)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S212(N2+1:M2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S212)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X64),size(X64),'3412',-1.000,
     & X64,S212)
       deallocate(S212)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S213(N2+1:M2,M1+1:N1,M1+1:N1,N2+1:M2))
       I1=K0*K7
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S213)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X66),size(X66),'3412',-1.000,
     & X66,S213)
       deallocate(S213)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S214(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S214)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X28(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       X28=0.0d0
       call sum_stripe(4,shape(X28),size(X28),'3412',-1.000,
     & X28,S214)
       deallocate(S214)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,M1,N1,X28,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z28(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X28,F2,Z28)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z28)
       deallocate(Z28)
       deallocate(X28)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S215(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S215)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X29(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X29=0.0d0
       call sum_stripe(4,shape(X29),size(X29),'3412',-1.000,
     & X29,S215)
       deallocate(S215)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,M1,N1,X29,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z29(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X29,F2,Z29)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & -1.000,V3C,Z29)
       deallocate(Z29)
       deallocate(X29)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(Q29(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q29)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X19),size(X19),'21',1.000,
     & X19,Q29)
       deallocate(Q29)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N2-N2,N0-N0,M1-M1/),'413256',t3C4,F2)
       allocate(S216(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S216)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S216)
       deallocate(S216)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N2-N2,N0-N0,M1-N0/),'513246',t3C1,F2)
       allocate(S217(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S217)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S217)
       deallocate(S217)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N2-N2,N0-N0,M1-M1/),'413256',t3C4,F2)
       allocate(S218(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S218)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S218)
       deallocate(S218)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N2-N2,N0-N0,M1-N0/),'513246',t3C1,F2)
       allocate(S219(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S219)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S219)
       deallocate(S219)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N2-N2,N0-N0,M1-M1/),'413256',t3C4,F2)
       allocate(S220(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S220)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S220)
       deallocate(S220)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N2-N2,N0-N0,M1-N0/),'513246',t3C1,F2)
       allocate(S221(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S221)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S221)
       deallocate(S221)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N2-N2,N0-N0,M1-M1/),'413256',t3C4,F2)
       allocate(S222(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S222)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S222)
       deallocate(S222)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N2-N2,N0-N0,M1-N0/),'513246',t3C1,F2)
       allocate(S223(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S223)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S223)
       deallocate(S223)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S224(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S224)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S224),size(S224),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,N0-N0/),'3412',S224,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'412356',t3C4,F2)
       allocate(Z330(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z330)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & -1.000,V3C,Z330)
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & 1.000,V3C,Z330)
       deallocate(Z330)
       deallocate(S224)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S225(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S225)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S225),size(S225),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,N0-N0/),'3412',S225,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N1-N1,N0-N0,M1-N0/),'512346',t3C1,F2)
       allocate(Z331(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z331)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & 1.000,V3C,Z331)
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & -1.000,V3C,Z331)
       deallocate(Z331)
       deallocate(S225)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S226(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S226)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S226),size(S226),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S226,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'412356',t3C4,F2)
       allocate(Z332(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z332)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & -1.000,V3C,Z332)
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & 1.000,V3C,Z332)
       deallocate(Z332)
       deallocate(S226)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S227(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S227)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S227),size(S227),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N2-N2,N0-N0/),'3412',S227,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N1-N1,N0-N0,M1-N0/),'512346',t3C1,F2)
       allocate(Z333(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z333)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & 1.000,V3C,Z333)
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & -1.000,V3C,Z333)
       deallocate(Z333)
       deallocate(S227)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N2-N2,N0-N0,M1-M1/),'413256',t3C4,F2)
       allocate(S228(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S228)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S228)
       deallocate(S228)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N2-N2,N0-N0,M1-N0/),'513246',t3C1,F2)
       allocate(S229(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S229)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S229)
       deallocate(S229)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N2-N2,N0-N0,M1-M1/),'413256',t3C4,F2)
       allocate(S230(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S230)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S230)
       deallocate(S230)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N2-N2,N0-N0,M1-N0/),'513246',t3C1,F2)
       allocate(S231(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S231)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S231)
       deallocate(S231)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N2-N2,N0-N0,M1-M1/),'413256',t3C4,F2)
       allocate(S232(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S232)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S232)
       deallocate(S232)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N2-N2,N0-N0,M1-N0/),'513246',t3C1,F2)
       allocate(S233(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S233)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S233)
       deallocate(S233)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N2-N2,N0-N0,M1-M1/),'413256',t3C4,F2)
       allocate(S234(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S234)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S234)
       deallocate(S234)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N2-N2,N0-N0,M1-N0/),'513246',t3C1,F2)
       allocate(S235(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S235)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S235)
       deallocate(S235)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S236(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S236)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S236),size(S236),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,N0-N0/),'3412',S236,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'412356',t3C4,F2)
       allocate(Z342(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z342)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & 1.000,V3C,Z342)
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & -1.000,V3C,Z342)
       deallocate(Z342)
       deallocate(S236)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S237(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S237)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S237),size(S237),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N2-N2,N0-N0/),'3412',S237,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N1-N1,N0-N0,M1-N0/),'512346',t3C1,F2)
       allocate(Z343(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z343)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & -1.000,V3C,Z343)
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & 1.000,V3C,Z343)
       deallocate(Z343)
       deallocate(S237)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S238(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S238)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S238),size(S238),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S238,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'412356',t3C4,F2)
       allocate(Z344(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z344)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & 1.000,V3C,Z344)
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & -1.000,V3C,Z344)
       deallocate(Z344)
       deallocate(S238)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S239(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S239)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S239),size(S239),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N2-N2,N0-N0/),'3412',S239,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N1-N1,N0-N0,M1-N0/),'512346',t3C1,F2)
       allocate(Z345(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z345)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & -1.000,V3C,Z345)
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & 1.000,V3C,Z345)
       deallocate(Z345)
       deallocate(S239)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S240(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S240)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X67),size(X67),'3412',1.000,
     & X67,S240)
       deallocate(S240)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S241(N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S241)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X68),size(X68),'3412',1.000,
     & X68,S241)
       deallocate(S241)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S242(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S242)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X69),size(X69),'3412',1.000,
     & X69,S242)
       deallocate(S242)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S243(N1+1:M2,N0+1:M1,M1+1:N2,N1+1:M2))
       I1=K9*K8
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S243)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X70),size(X70),'3412',1.000,
     & X70,S243)
       deallocate(S243)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(Q30(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q30)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X71),size(X71),'21',1.000,
     & X71,Q30)
       deallocate(Q30)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'4123',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(Q31(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q31)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X72),size(X72),'21',1.000,
     & X72,Q31)
       deallocate(Q31)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S244(N2+1:M2,N1+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S244)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X41),size(X41),'3412',1.000,
     & X41,S244)
       deallocate(S244)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S245(N2+1:M2,N1+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S245)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X42),size(X42),'3412',1.000,
     & X42,S245)
       deallocate(S245)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S246(N2+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S246)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X43),size(X43),'3412',1.000,
     & X43,S246)
       deallocate(S246)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S247(N2+1:M2,N1+1:M2,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S247)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X44),size(X44),'3412',1.000,
     & X44,S247)
       deallocate(S247)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',t2B,D2)
       allocate(Q32(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q32)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X73),size(X73),'21',-1.000,
     & X73,Q32)
       deallocate(Q32)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',t2B,D2)
       allocate(Q33(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q33)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X75),size(X75),'21',-1.000,
     & X75,Q33)
       deallocate(Q33)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S248(N2+1:M2,N1+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S248)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X45),size(X45),'3412',1.000,
     & X45,S248)
       deallocate(S248)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S249(N2+1:M2,N1+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S249)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X46),size(X46),'3412',1.000,
     & X46,S249)
       deallocate(S249)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S250(N2+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S250)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X47),size(X47),'3412',1.000,
     & X47,S250)
       deallocate(S250)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S251(N2+1:M2,N1+1:M2,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S251)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X48),size(X48),'3412',1.000,
     & X48,S251)
       deallocate(S251)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',t2B,D2)
       allocate(Q34(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q34)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X74),size(X74),'21',-1.000,
     & X74,Q34)
       deallocate(Q34)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',t2B,D2)
       allocate(Q35(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q35)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X76),size(X76),'21',-1.000,
     & X76,Q35)
       deallocate(Q35)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(Q36(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q36)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X20),size(X20),'21',-1.000,
     & X20,Q36)
       deallocate(Q36)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(Q37(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q37)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X21),size(X21),'21',-1.000,
     & X21,Q37)
       deallocate(Q37)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,N2-N2,N2-N2,N0-N0/),'461235',t3D,F2)
       allocate(S252(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S252)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S252)
       deallocate(S252)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,N2-N2,N2-N2,N0-N0/),'561234',t3D,F2)
       allocate(S253(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S253)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',-0.500,
     & X7,S253)
       deallocate(S253)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,N2-N2,N2-N2,N0-N0/),'461235',t3D,F2)
       allocate(S254(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S254)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S254)
       deallocate(S254)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,N2-N2,N2-N2,N0-N0/),'561234',t3D,F2)
       allocate(S255(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S255)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',-0.500,
     & X7,S255)
       deallocate(S255)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(S256(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S256)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-0.500,
     & X5,S256)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-0.500,
     & X6,S256)
       deallocate(S256)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(S258(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S258)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-0.500,
     & X5,S258)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-0.500,
     & X6,S258)
       deallocate(S258)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(S257(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S257)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S257)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S257)
       deallocate(S257)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S259(N1+1:M2,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S259)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X81),size(X81),'3412',1.000,
     & X81,S259)
       deallocate(S259)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S260(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S260)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X82),size(X82),'3412',1.000,
     & X82,S260)
       deallocate(S260)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S261(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S261)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S261)
       deallocate(S261)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S262(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S262)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S262)
       deallocate(S262)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S263(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S263)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S263)
       deallocate(S263)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S264(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S264)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S264)
       deallocate(S264)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S265(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S265)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S265)
       deallocate(S265)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N1-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S266(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S266)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S266)
       deallocate(S266)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S267(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S267)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S267)
       deallocate(S267)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S268(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S268)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S268)
       deallocate(S268)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S269(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S269)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S269)
       deallocate(S269)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S270(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S270)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S270)
       deallocate(S270)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S271(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S271)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S271)
       deallocate(S271)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S272(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S272)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S272)
       deallocate(S272)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S273(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S273)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S273)
       deallocate(S273)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N1-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S274(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S274)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S274)
       deallocate(S274)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S275(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S275)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S275)
       deallocate(S275)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,N2-N2,N1-N1,M1-M1/),'452136',t3B1,F2)
       allocate(S276(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S276)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S276)
       deallocate(S276)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S277(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S277)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S277)
       deallocate(S277)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S278(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S278)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S278)
       deallocate(S278)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S279(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S279)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S279)
       deallocate(S279)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S280(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S280)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S280)
       deallocate(S280)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S281(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S281)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S281)
       deallocate(S281)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S282(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S282)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S282)
       deallocate(S282)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S283(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S283)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S283)
       deallocate(S283)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'512346',t3B1,F2)
       allocate(S284(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S284)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S284)
       deallocate(S284)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S285(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S285)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'3412',1.000,
     & X11,S285)
       deallocate(S285)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S286(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S286)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'3412',1.000,
     & X12,S286)
       deallocate(S286)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S287(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S287)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X13),size(X13),'3412',1.000,
     & X13,S287)
       deallocate(S287)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S288(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S288)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X14),size(X14),'3412',1.000,
     & X14,S288)
       deallocate(S288)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S289(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S289)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X15),size(X15),'3412',1.000,
     & X15,S289)
       deallocate(S289)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S290(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S290)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X16),size(X16),'3412',1.000,
     & X16,S290)
       deallocate(S290)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S291(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S291)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X17),size(X17),'3412',1.000,
     & X17,S291)
       deallocate(S291)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S292(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S292)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X18),size(X18),'3412',1.000,
     & X18,S292)
       deallocate(S292)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2,N1-N1,M1-M1/),'451236',t3C4,F2)
       allocate(S293(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S293)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',0.500,
     & X10,S293)
       deallocate(S293)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(S294(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S294)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',1.000,
     & X10,S294)
       deallocate(S294)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(S295(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S295)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',0.500,
     & X10,S295)
       deallocate(S295)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2,N1-N1,M1-M1/),'451236',t3C4,F2)
       allocate(S296(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S296)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',0.500,
     & X10,S296)
       deallocate(S296)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(S297(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S297)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',1.000,
     & X10,S297)
       deallocate(S297)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(S298(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S298)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',0.500,
     & X10,S298)
       deallocate(S298)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,N2-N2,N1-N1,M1-M1/),'451236',t3C4,F2)
       allocate(S299(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S299)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',0.500,X9,
     & S299)
       deallocate(S299)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(S300(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S300)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',1.000,X9,
     & S300)
       deallocate(S300)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(S301(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S301)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',0.500,X9,
     & S301)
       deallocate(S301)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2,N1-N1,M1-M1/),'451236',t3C4,F2)
       allocate(S302(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S302)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',0.500,X9,
     & S302)
       deallocate(S302)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(S303(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S303)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',1.000,X9,
     & S303)
       deallocate(S303)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(S304(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S304)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',0.500,X9,
     & S304)
       deallocate(S304)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S305(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K5*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S305)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X49(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       X49=0.0d0
       call sum_stripe(4,shape(X49),size(X49),'3412',0.500,
     & X49,S305)
       deallocate(S305)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,M1-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S306(N0+1:M1,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K5*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S306)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X50(N0+1:M1,M1+1:N2,N0+1:M1,N0+1:M1))
       X50=0.0d0
       call sum_stripe(4,shape(X50),size(X50),'3412',0.500,
     & X50,S306)
       deallocate(S306)
C
       allocate(D1(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,M1-N0,M1-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S307(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K5*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S307)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X51(M1+1:N2,M1+1:N2,N0+1:M1,N0+1:M1))
       X51=0.0d0
       call sum_stripe(4,shape(X51),size(X51),'3412',0.500,
     & X51,S307)
       deallocate(S307)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C2),size(t3C2),shape(F2),size(F2),
     & (/N0-N0,M2-M2,M2-M2,N1-N1,N0-N0,M1-M1/),'412356',t3C2,F2)
       allocate(S308(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S308)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-0.500,
     & X8,S308)
       deallocate(S308)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C3),size(t3C3),shape(F2),size(F2),
     & (/M1-M1,M2-M2,M2-M2,N1-N1,N0-N0,M1-N0/),'512346',t3C3,F2)
       allocate(S309(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S309)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',0.500,X8,
     & S309)
       deallocate(S309)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'412356',t3C4,F2)
       allocate(S310(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S310)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S310)
       deallocate(S310)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N1-N1,N0-N0,M1-N0/),'512346',t3C1,F2)
       allocate(S311(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S311)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S311)
       deallocate(S311)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'412356',t3C4,F2)
       allocate(S312(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S312)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-0.500,
     & X8,S312)
       deallocate(S312)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N2-N2,N0-N0/),'4123',VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N1-N1,N0-N0,M1-N0/),'512346',t3C1,F2)
       allocate(S313(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S313)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',0.500,X8,
     & S313)
       deallocate(S313)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S314(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S314)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X52),size(X52),'3412',-1.000,
     & X52,S314)
       deallocate(S314)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S315(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S315)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X53),size(X53),'3412',-1.000,
     & X53,S315)
       deallocate(S315)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S316(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S316)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X54),size(X54),'3412',-1.000,
     & X54,S316)
       deallocate(S316)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S317(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S317)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X55),size(X55),'3412',-1.000,
     & X55,S317)
       deallocate(S317)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S318(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S318)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X56),size(X56),'3412',-1.000,
     & X56,S318)
       deallocate(S318)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S319(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S319)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X57),size(X57),'3412',-1.000,
     & X57,S319)
       deallocate(S319)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S320(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S320)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X58),size(X58),'3412',-1.000,
     & X58,S320)
       deallocate(S320)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S321(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S321)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X59),size(X59),'3412',-1.000,
     & X59,S321)
       deallocate(S321)
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
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S322(N2+1:M2,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S322)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X60),size(X60),'3412',0.500,
     & X60,S322)
       deallocate(S322)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S323(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S323)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X61),size(X61),'3412',0.500,
     & X61,S323)
       deallocate(S323)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S324(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S324)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X62),size(X62),'3412',0.500,
     & X62,S324)
       deallocate(S324)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N2/),'3412',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(Q40(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
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
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(Q41(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q41)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X25),size(X25),'21',-0.500,
     & X25,Q41)
       deallocate(Q41)
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
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N0-N0/),'2341',VBHHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S325(M1+1:N1,N0+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S325)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S325),size(S325),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2341',S325,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S337(N2+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S337)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S337)
       deallocate(S337)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S325),size(S325),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2341',S325,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S338(N2+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S338)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S338)
       deallocate(S338)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S325),size(S325),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3241',S325,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S326(N1+1:M2,N0+1:N2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S326)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',-1.000,
     & X8,S326)
       deallocate(S326)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,M2-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S329(M1+1:N1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S329)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S329),size(S329),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'3241',S329,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S330(N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S330)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X79),size(X79),'3124',-1.000,
     & X79,S330)
       deallocate(S330)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z210(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X79,F2,Z210)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z210)
       deallocate(Z210)
       deallocate(X79)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S331(M1+1:N1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S331)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S331),size(S331),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'3241',S331,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S332(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S332)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X80),size(X80),'3124',-1.000,
     & X80,S332)
       deallocate(S332)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N2-N2,N2-N2,N0-N0,N0-N0/),'631245',t3C4,F2)
       allocate(Z211(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X80,F2,Z211)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z211)
       deallocate(Z211)
       deallocate(X80)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q44(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(Q44),size(Q44),shape(B1),size(B1),
     & (/N1-N1,M1-M1/),'21',Q44,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(Q45(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q45)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X19),size(X19),'21',1.000,
     & X19,Q45)
       deallocate(Q45)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q46(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q47(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q46,B2,Q47)
       deallocate(B2)
C
       call sum_stripe(2,shape(X20),size(X20),'21',-1.000,
     & X20,Q47)
       deallocate(Q47)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q48(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q49(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q48,B2,Q49)
       deallocate(B2)
C
       call sum_stripe(2,shape(X21),size(X21),'21',-1.000,
     & X21,Q49)
       deallocate(Q49)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S333(M1+1:N1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S333)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S333),size(S333),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'3241',S333,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S334(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S334)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X81),size(X81),'3124',-1.000,
     & X81,S334)
       deallocate(S334)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z215(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X81,F2,Z215)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z215)
       deallocate(Z215)
       deallocate(X81)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S335(M1+1:N1,M1+1:N2,N0+1:N1,N2+1:M2))
       I1=K0*K1*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S335)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S335),size(S335),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'3241',S335,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S336(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S336)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X82),size(X82),'3124',-1.000,
     & X82,S336)
       deallocate(S336)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z216(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,X82,F2,Z216)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z216)
       deallocate(Z216)
       deallocate(X82)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,N2-N2/),'2413',VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S327(M1+1:N1,N0+1:N1,N2+1:N3,N2+1:M2))
       I1=K0*K4*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S327)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S327),size(S327),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,M1-M1/),'3241',S327,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S339(N0+1:M1,N0+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K1
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S339)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'3124',1.000,X1,
     & S339)
       deallocate(S339)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S327),size(S327),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,M1-M1/),'3241',S327,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S340(N0+1:M1,N0+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K1
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S340)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'3124',1.000,X2,
     & S340)
       deallocate(S340)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S327),size(S327),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'2341',S327,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S328(N1+1:M2,N2+1:N3,N2+1:M2,M1+1:N1))
       I1=K7*K0*K4
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S328)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'3124',-1.000,
     & X9,S328)
C
       call sum_stripe(4,shape(X10),size(X10),'3124',-1.000,
     & X10,S328)
       deallocate(S328)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'4321',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S341(N1+1:M2,N0+1:N2,N1+1:N3,N0+1:M1))
       I1=K5*K3*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S341)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S341),size(S341),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2314',S341,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S343(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S343)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',1.000,X4,
     & S343)
       deallocate(S343)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S341),size(S341),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2314',S341,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S342(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S342)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',1.000,X3,
     & S342)
       deallocate(S342)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S344(N0+1:M1,N0+1:N1,N1+1:N3,N2+1:M2))
       I1=K0*K3*K1
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S344)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S344),size(S344),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'2341',S344,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S345(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S345)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'3124',-1.000,
     & X3,S345)
       deallocate(S345)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S346(N0+1:M1,N0+1:N1,N1+1:N3,N2+1:M2))
       I1=K0*K3*K1
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S346)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S346),size(S346),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'2341',S346,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S347(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S347)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'3124',-1.000,
     & X4,S347)
       deallocate(S347)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M1-N0/),'1342',VBHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S348(N0+1:M1,N0+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S348)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S348),size(S348),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S348,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S417(N2+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S417)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S417)
       deallocate(S417)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S348),size(S348),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S348,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S418(N2+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S418)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S418)
       deallocate(S418)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S348),size(S348),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3214',S348,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S349(N1+1:M2,N0+1:N2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S349)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',-1.000,
     & X8,S349)
       deallocate(S349)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S353(M1+1:N1,N0+1:N2,N2+1:N3,N1+1:M2))
       I1=K9*K4*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S353)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S353),size(S353),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'2341',S353,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S355(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S355)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',-1.000,
     & X9,S355)
       deallocate(S355)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S353),size(S353),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'2341',S353,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S356(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S356)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',-1.000,
     & X10,S356)
       deallocate(S356)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S353),size(S353),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N1-N1,M1-M1/),'3241',S353,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S354(N0+1:M1,N0+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S354)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'3124',1.000,X8,
     & S354)
       deallocate(S354)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S357(M1+1:N1,M1+1:N2,N0+1:M1,N2+1:N3))
       I1=K4*K5*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S357)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S357),size(S357),shape(D1),size(D1),
     & (/N2-N2,M1-M1,N0-N0,M1-M1/),'4231',S357,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S358(N0+1:M1,M1+1:N2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S358)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X30),size(X30),'3124',1.000,
     & X30,S358)
       deallocate(S358)
C
       call sumx_sorted3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,N0,M1,M1,N1,X30,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(Z30(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K5*K9*K0*K0
       I3=K5*K8
       call EGEMM(I1,I2,I3,X30,F2,Z30)
       deallocate(F2)
C
       V3C=V3C+Z30
       call sum_stripe(6,shape(V3C),size(V3C),'123546',
     & -1.000,V3C,Z30)
       deallocate(Z30)
       deallocate(X30)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S359(M1+1:N1,N0+1:M1,M1+1:N1,N2+1:N3))
       I1=K4*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S359)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S359),size(S359),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M1-M1,M1-M1/),'4231',S359,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S360(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S360)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X31),size(X31),'3124',1.000,
     & X31,S360)
       deallocate(S360)
C
       call sumx_sorted3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,N0,M1,M1,N1,X31,VBHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,N2-N2,N1-N1,N0-N0/),'461235',t3C4,F2)
       allocate(Z31(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K5*K9*K0*K0
       I3=K7*K5
       call EGEMM(I1,I2,I3,X31,F2,Z31)
       deallocate(F2)
C
       V3C=V3C-Z31
       call sum_stripe(6,shape(V3C),size(V3C),'123546',
     & 1.000,V3C,Z31)
       deallocate(Z31)
       deallocate(X31)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S361(M1+1:N1,M1+1:N2,M1+1:N1,N2+1:N3))
       I1=K4*K7*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S361)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S361),size(S361),shape(D1),size(D1),
     & (/N2-N2,M1-M1,M1-M1,M1-M1/),'4231',S361,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S362(N0+1:M1,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S362)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X32),size(X32),'3124',1.000,
     & X32,S362)
       deallocate(S362)
C
       call sumx_sorted3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,N0,M1,M1,N1,X32,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M1-N0,N2-N2,N2-N2,N1-N1,N0-N0/),'561234',t3C1,F2)
       allocate(Z32(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K5*K9*K0*K0
       I3=K7*K8
       call EGEMM(I1,I2,I3,X32,F2,Z32)
       deallocate(F2)
C
       V3C=V3C+Z32
       call sum_stripe(6,shape(V3C),size(V3C),'123546',
     & -1.000,V3C,Z32)
       deallocate(Z32)
       deallocate(X32)
C
       allocate(D1(N1+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S363(M1+1:N1,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S363)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S363),size(S363),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S363,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S367(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S367)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X64),size(X64),'3124',-1.000,
     & X64,S367)
       deallocate(S367)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'612345',t3C4,F2)
       allocate(Z90(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K5*K5*K9*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X64,F2,Z90)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134526',
     & 1.000,V3C,Z90)
       deallocate(Z90)
       deallocate(X64)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S363),size(S363),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S363,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S364(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S364)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X63),size(X63),'3124',-1.000,
     & X63,S364)
       deallocate(S364)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'612345',t3C4,F2)
       allocate(Z88(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K5*K5*K9*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X63,F2,Z88)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234516',
     & -1.000,V3C,Z88)
       deallocate(Z88)
       deallocate(X63)
C
       allocate(D1(N1+1:N3,N0+1:N2,M1+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S365(M1+1:N1,N0+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S365)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S365),size(S365),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'2341',S365,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S368(N2+1:M2,M1+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S368)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X66),size(X66),'3124',-1.000,
     & X66,S368)
       deallocate(S368)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'612345',t3C4,F2)
       allocate(Z91(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K5*K5*K9*K0
       I3=K0*K7
       call EGEMM(I1,I2,I3,X66,F2,Z91)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134526',
     & 1.000,V3C,Z91)
       deallocate(Z91)
       deallocate(X66)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S365),size(S365),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'2341',S365,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S366(N2+1:M2,M1+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S366)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X65),size(X65),'3124',-1.000,
     & X65,S366)
       deallocate(S366)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'612345',t3C4,F2)
       allocate(Z89(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K5*K5*K9*K0
       I3=K0*K7
       call EGEMM(I1,I2,I3,X65,F2,Z89)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234516',
     & -1.000,V3C,Z89)
       deallocate(Z89)
       deallocate(X65)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q50(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q50)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(Q50),size(Q50),shape(B1),size(B1),
     & (/N1-N1,M1-M1/),'21',Q50,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(Q51(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q51)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X19),size(X19),'21',1.000,
     & X19,Q51)
       deallocate(Q51)
C
       call sumx_sorted21(N0,N1,N0,N1,
     & M1,N1,M1,N1,X19,FAHH, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N1-N1,N0-N0,N0-N0/),'612345',t3C4,F2)
       allocate(Z19(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K5*K9*K0*K0
       I3=K7
       call EGEMM(I1,I2,I3,X19,F2,Z19)
       deallocate(F2)
C
       V3C=V3C-Z19
       deallocate(Z19)
       deallocate(X19)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S369(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S369)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S369),size(S369),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'3241',S369,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S370(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S370)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X67),size(X67),'3124',1.000,
     & X67,S370)
       deallocate(S370)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N2-N2,N2-N2,N0-N0,M1-M1/),'431256',t3C4,F2)
       allocate(Z92(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X67,F2,Z92)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124635',
     & -1.000,V3C,Z92)
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & 1.000,V3C,Z92)
       deallocate(Z92)
       deallocate(X67)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S371(N0+1:M1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S371)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S371),size(S371),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'3241',S371,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S372(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S372)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X68),size(X68),'3124',1.000,
     & X68,S372)
       deallocate(S372)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N2-N2,N2-N2,N0-N0,M1-N0/),'531246',t3C1,F2)
       allocate(Z93(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X68,F2,Z93)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124635',
     & 1.000,V3C,Z93)
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & -1.000,V3C,Z93)
       deallocate(Z93)
       deallocate(X68)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S373(N0+1:M1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S373)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S373),size(S373),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'3241',S373,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S374(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S374)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X69),size(X69),'3124',1.000,
     & X69,S374)
       deallocate(S374)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N2-N2,N2-N2,N0-N0,M1-M1/),'431256',t3C4,F2)
       allocate(Z94(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,X69,F2,Z94)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124635',
     & -1.000,V3C,Z94)
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & 1.000,V3C,Z94)
       deallocate(Z94)
       deallocate(X69)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S375(N0+1:M1,M1+1:N2,N0+1:N1,N1+1:M2))
       I1=K9*K1*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S375)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S375),size(S375),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,N0-N0/),'3241',S375,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S376(N1+1:M2,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S376)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X70),size(X70),'3124',1.000,
     & X70,S376)
       deallocate(S376)
C
       allocate(F2(M1+1:N2,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N2-N2,N2-N2,N0-N0,M1-N0/),'531246',t3C1,F2)
       allocate(Z95(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K0*K0
       I3=K9*K8
       call EGEMM(I1,I2,I3,X70,F2,Z95)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124635',
     & 1.000,V3C,Z95)
       call sum_stripe(6,shape(V3C),size(V3C),'125634',
     & -1.000,V3C,Z95)
       deallocate(Z95)
       deallocate(X70)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q52(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q52)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q52),size(Q52),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q52,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q53(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q53)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X71),size(X71),'21',1.000,
     & X71,Q53)
       deallocate(Q53)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'412356',t3C4,F2)
       allocate(Z96(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K5*K9*K0*K0
       I3=K5
       call EGEMM(I1,I2,I3,X71,F2,Z96)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123465',
     & 1.000,V3C,Z96)
       call sum_stripe(6,shape(V3C),size(V3C),'123564',
     & -1.000,V3C,Z96)
       deallocate(Z96)
       deallocate(X71)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q54(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q54)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(Q54),size(Q54),shape(B1),size(B1),
     & (/N2-N2,M1-M1/),'21',Q54,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q55(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q55)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X72),size(X72),'21',1.000,
     & X72,Q55)
       deallocate(Q55)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N1-N1,N0-N0,M1-N0/),'512346',t3C1,F2)
       allocate(Z97(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K5*K9*K0*K0
       I3=K8
       call EGEMM(I1,I2,I3,X72,F2,Z97)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123465',
     & -1.000,V3C,Z97)
       call sum_stripe(6,shape(V3C),size(V3C),'123564',
     & 1.000,V3C,Z97)
       deallocate(Z97)
       deallocate(X72)
C
       allocate(D1(N0+1:N1,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S377(N1+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S377)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S377),size(S377),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N1-N1/),'2341',S377,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S385(N2+1:M2,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S385)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X45),size(X45),'3124',1.000,
     & X45,S385)
       deallocate(S385)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,M2,N3,N2,M2,N1,M2,X45,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M2-N2,M2-N1,N2-N2,N0-N0,N0-N0,M1-M1/),'132456',t3C4,F2)
       allocate(Z45(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K7*K5*K5*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,X45,F2,Z45)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'145623',
     & -1.000,V3C,Z45)
       deallocate(Z45)
       deallocate(X45)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S377),size(S377),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N1-N1/),'2341',S377,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S378(N2+1:M2,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S378)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X41),size(X41),'3124',1.000,
     & X41,S378)
       deallocate(S378)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,M2,N3,N2,M2,N1,M2,X41,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M2-N2,M2-N1,N2-N2,N0-N0,N0-N0,M1-M1/),'132456',t3C4,F2)
       allocate(Z41(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K7*K5*K5*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,X41,F2,Z41)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'245613',
     & 1.000,V3C,Z41)
       deallocate(Z41)
       deallocate(X41)
C
       allocate(D1(N0+1:N1,N0+1:N2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S381(N1+1:M2,N0+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S381)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S381),size(S381),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N1-N1/),'2341',S381,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S387(N2+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S387)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X47),size(X47),'3124',1.000,
     & X47,S387)
       deallocate(S387)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,N1,M2,N2,M2,N1,M2,X47,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M2-N2,N1-N1,N2-N2,N0-N0,N0-N0,M1-M1/),'132456',t3C4,F2)
       allocate(Z47(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K7*K5*K5*K0
       I3=K9*K6
       call EGEMM(I1,I2,I3,X47,F2,Z47)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'145623',
     & -1.000,V3C,Z47)
       deallocate(Z47)
       deallocate(X47)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S381),size(S381),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N1-N1/),'2341',S381,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S382(N2+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S382)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X43),size(X43),'3124',1.000,
     & X43,S382)
       deallocate(S382)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,N1,M2,N2,M2,N1,M2,X43,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M2-N2,N1-N1,N2-N2,N0-N0,N0-N0,M1-M1/),'132456',t3C4,F2)
       allocate(Z43(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K7*K5*K5*K0
       I3=K9*K6
       call EGEMM(I1,I2,I3,X43,F2,Z43)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'245613',
     & 1.000,V3C,Z43)
       deallocate(Z43)
       deallocate(X43)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q56(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q56)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q60(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q56,B2,Q60)
       deallocate(B2)
C
       call sum_stripe(2,shape(X74),size(X74),'21',-1.000,
     & X74,Q60)
       deallocate(Q60)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3C4,F2)
       allocate(Z108(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K5*K5*K9*K0
       I3=K6
       call EGEMM(I1,I2,I3,X74,F2,Z108)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134562',
     & -1.000,V3C,Z108)
       deallocate(Z108)
       deallocate(X74)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q57(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q56,B2,Q57)
       deallocate(B2)
C
       call sum_stripe(2,shape(X73),size(X73),'21',-1.000,
     & X73,Q57)
       deallocate(Q57)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3C4,F2)
       allocate(Z102(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K5*K5*K9*K0
       I3=K6
       call EGEMM(I1,I2,I3,X73,F2,Z102)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234561',
     & 1.000,V3C,Z102)
       deallocate(Z102)
       deallocate(X73)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S379(N1+1:M2,N0+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S379)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S379),size(S379),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,N1-N1/),'2341',S379,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S386(N2+1:M2,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S386)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X46),size(X46),'3124',1.000,
     & X46,S386)
       deallocate(S386)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,M2,N3,N2,M2,N1,M2,X46,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N2-N2,M2-N1,N2-N2,N0-N0,N0-N0,M1-M1/),'132456',t3C4,F2)
       allocate(Z46(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K7*K5*K5*K0
       I3=K6*K0
       call EGEMM(I1,I2,I3,X46,F2,Z46)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'145623',
     & -1.000,V3C,Z46)
       deallocate(Z46)
       deallocate(X46)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S379),size(S379),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,N1-N1/),'2341',S379,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S380(N2+1:M2,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S380)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X42),size(X42),'3124',1.000,
     & X42,S380)
       deallocate(S380)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,M2,N3,N2,M2,N1,M2,X42,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N2-N2,M2-N1,N2-N2,N0-N0,N0-N0,M1-M1/),'132456',t3C4,F2)
       allocate(Z42(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K7*K5*K5*K0
       I3=K6*K0
       call EGEMM(I1,I2,I3,X42,F2,Z42)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'245613',
     & 1.000,V3C,Z42)
       deallocate(Z42)
       deallocate(X42)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S383(N1+1:M2,N0+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S383)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S383),size(S383),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N1-N1/),'2341',S383,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S388(N2+1:M2,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S388)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X48),size(X48),'3124',1.000,
     & X48,S388)
       deallocate(S388)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,N1,M2,N2,M2,N1,M2,X48,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N2-N2,N1-N1,N2-N2,N0-N0,N0-N0,M1-M1/),'132456',t3C4,F2)
       allocate(Z48(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K7*K5*K5*K0
       I3=K9*K0
       call EGEMM(I1,I2,I3,X48,F2,Z48)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'145623',
     & -1.000,V3C,Z48)
       deallocate(Z48)
       deallocate(X48)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S383),size(S383),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N1-N1/),'2341',S383,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S384(N2+1:M2,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S384)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X44),size(X44),'3124',1.000,
     & X44,S384)
       deallocate(S384)
C
       call sumx_sorted1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,N1,M2,N2,M2,N1,M2,X44,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N2-N2,N1-N1,N2-N2,N0-N0,N0-N0,M1-M1/),'132456',t3C4,F2)
       allocate(Z44(N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K7*K5*K5*K0
       I3=K9*K0
       call EGEMM(I1,I2,I3,X44,F2,Z44)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'245613',
     & 1.000,V3C,Z44)
       deallocate(Z44)
       deallocate(X44)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q58(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q58)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q61(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q58,B2,Q61)
       deallocate(B2)
C
       call sum_stripe(2,shape(X76),size(X76),'21',-1.000,
     & X76,Q61)
       deallocate(Q61)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3C4,F2)
       allocate(Z109(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K5*K5*K9*K0
       I3=K0
       call EGEMM(I1,I2,I3,X76,F2,Z109)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134562',
     & -1.000,V3C,Z109)
       deallocate(Z109)
       deallocate(X76)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q59(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q58,B2,Q59)
       deallocate(B2)
C
       call sum_stripe(2,shape(X75),size(X75),'21',-1.000,
     & X75,Q59)
       deallocate(Q59)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3C4,F2)
       allocate(Z103(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K5*K5*K9*K0
       I3=K0
       call EGEMM(I1,I2,I3,X75,F2,Z103)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234561',
     & 1.000,V3C,Z103)
       deallocate(Z103)
       deallocate(X75)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q62(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q62)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q63(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q62,B2,Q63)
       deallocate(B2)
C
       call sum_stripe(2,shape(X20),size(X20),'21',-1.000,
     & X20,Q63)
       deallocate(Q63)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & M2,N3,N1,M2,X20,FAPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M2-N1,N2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'312456',t3C4,F2)
       allocate(Z20(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K5*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,X20,F2,Z20)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124563',
     & 1.000,V3C,Z20)
       deallocate(Z20)
       deallocate(X20)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q64(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q64)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q65(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q64,B2,Q65)
       deallocate(B2)
C
       call sum_stripe(2,shape(X21),size(X21),'21',-1.000,
     & X21,Q65)
       deallocate(Q65)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & N1,M2,N1,M2,X21,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N1-N1,N2-N2,N2-N2,N0-N0,N0-N0,M1-M1/),'312456',t3C4,F2)
       allocate(Z21(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K5*K0*K0
       I3=K9
       call EGEMM(I1,I2,I3,X21,F2,Z21)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124563',
     & 1.000,V3C,Z21)
       deallocate(Z21)
       deallocate(X21)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S389(M1+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S389)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S389),size(S389),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'2431',S389,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S391(N2+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S391)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S391)
       deallocate(S391)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S389),size(S389),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'2431',S389,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S390(N2+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S390)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2314',1.000,X2,
     & S390)
       deallocate(S390)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S392(N2+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S392)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S392),size(S392),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3412',S392,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S393(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S393)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'3124',-1.000,
     & X4,S393)
       deallocate(S393)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q66(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q66)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q66),size(Q66),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q66,B1)
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,N0-N0,M1-N0/),'2134',t2B,D2)
       allocate(S397(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S397)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S397)
       deallocate(S397)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q66),size(Q66),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q66,B1)
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,N0-N0,M1-N0/),'2134',t2B,D2)
       allocate(S394(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S394)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S394)
       deallocate(S394)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S395(N2+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S395)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S395),size(S395),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3412',S395,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S396(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S396)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'3124',-1.000,
     & X3,S396)
       deallocate(S396)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S398(M1+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S398)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S398),size(S398),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S398,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S400(N2+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S400)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S400)
       deallocate(S400)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,M1+1:N1))
       call reorder_shift(4,shape(S398),size(S398),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'3421',S398,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S401(N1+1:M2,N0+1:M1,N0+1:N2,M1+1:N1))
       I1=K7*K2
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S401)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2314',-1.000,
     & X8,S401)
       deallocate(S401)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S398),size(S398),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'2341',S398,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S402(N2+1:M2,N1+1:M2,N2+1:N3,M1+1:N1))
       I1=K7*K4
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S402)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2314',1.000,X9,
     & S402)
       deallocate(S402)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S398),size(S398),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'2341',S398,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S403(N2+1:M2,N1+1:M2,N2+1:N3,M1+1:N1))
       I1=K7*K4
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S403)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2314',1.000,
     & X10,S403)
       deallocate(S403)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S398),size(S398),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N0-N0,M1-M1/),'4231',S398,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S509(N0+1:M1,N0+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S509)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S509),size(S509),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3214',S509,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S510(N1+1:M2,N0+1:N2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S510)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',-1.000,
     & X8,S510)
       deallocate(S510)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S398),size(S398),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,M1-M1/),'3241',S398,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S511(N1+1:M2,N0+1:N2,N2+1:N3,M1+1:N1))
       I1=K7*K4*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S511)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S511),size(S511),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'2314',S511,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S512(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S512)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',1.000,X9,
     & S512)
       deallocate(S512)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S398),size(S398),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S398,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S399(N2+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S399)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2314',1.000,X2,
     & S399)
       deallocate(S399)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S509),size(S509),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S509,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S514(N2+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S514)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S514)
       deallocate(S514)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S509),size(S509),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S509,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S515(N2+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S515)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S515)
       deallocate(S515)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S511),size(S511),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'2314',S511,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S513(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S513)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',1.000,
     & X10,S513)
       deallocate(S513)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q67(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q67),size(Q67),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q67,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S416(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S416)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S416)
       deallocate(S416)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q67),size(Q67),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q67,B1)
       allocate(D2(N2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,M1-N0/),'1234',t2B,D2)
       allocate(S410(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S410)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S410)
       deallocate(S410)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q67),size(Q67),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q67,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S413(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S413)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S413)
       deallocate(S413)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S350(N1+1:M2,N0+1:N2,N2+1:N3,M1+1:N1))
       I1=K7*K4*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S350)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S350),size(S350),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'2314',S350,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S352(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S352)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',1.000,
     & X10,S352)
       deallocate(S352)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S350),size(S350),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'2314',S350,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S351(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S351)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',1.000,X9,
     & S351)
       deallocate(S351)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N1-N1/),'1324',VBPHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S419(N0+1:M1,N0+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S419)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S419),size(S419),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2341',S419,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S421(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S421)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',-1.000,
     & X4,S421)
       deallocate(S421)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S419),size(S419),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2341',S419,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S420(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S420)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',-1.000,
     & X3,S420)
       deallocate(S420)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N0-N0/),'1342',VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S422(N0+1:M1,N0+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S422)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S422),size(S422),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S422,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S424(N2+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S424)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X78),size(X78),'2134',-1.000,
     & X78,S424)
       deallocate(S424)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S422),size(S422),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S422,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S423(N2+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S423)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X77),size(X77),'2134',-1.000,
     & X77,S423)
       deallocate(S423)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'4312',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S425(N2+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S425)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S425),size(S425),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2314',S425,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S426(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S426)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',1.000,X7,
     & S426)
       deallocate(S426)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'2413',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S427(N0+1:M1,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S427)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S427),size(S427),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2341',S427,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S431(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S431)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'3124',-1.000,
     & X7,S431)
       deallocate(S431)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S427),size(S427),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,N0-N0/),'3241',S427,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S428(N0+1:M1,N0+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S428)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'3124',1.000,X5,
     & S428)
       deallocate(S428)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'2413',VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S429(N0+1:M1,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S429)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S429),size(S429),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2341',S429,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S432(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S432)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',1.000,X7,
     & S432)
       deallocate(S432)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S429),size(S429),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,N0-N0/),'3241',S429,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S430(N0+1:M1,N0+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S430)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'3124',1.000,X6,
     & S430)
       deallocate(S430)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S433(N0+1:M1,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S433)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S433),size(S433),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S433,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S441(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S441)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X15),size(X15),'3124',-1.000,
     & X15,S441)
       deallocate(S441)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,N0,M1,X15,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N2-N2,N1-N1,N0-N0,M1-M1/),'521346',t3B1,F2)
       allocate(Z15(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X15,F2,Z15)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & 1.000,V3C,Z15)
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & -1.000,V3C,Z15)
       deallocate(Z15)
       deallocate(X15)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S433),size(S433),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S433,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S434(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S434)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'3124',-1.000,
     & X11,S434)
       deallocate(S434)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,N0,M1,X11,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N2-N2,N1-N1,N0-N0,M1-M1/),'521346',t3B1,F2)
       allocate(Z11(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X11,F2,Z11)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & -1.000,V3C,Z11)
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & 1.000,V3C,Z11)
       deallocate(Z11)
       deallocate(X11)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S437(N0+1:M1,N0+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S437)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S437),size(S437),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S437,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S443(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S443)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X17),size(X17),'3124',-1.000,
     & X17,S443)
       deallocate(S443)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,N2,M2,N0,M1,X17,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N2-N2,N1-N1,N0-N0,M1-M1/),'521346',t3B1,F2)
       allocate(Z17(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,X17,F2,Z17)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & 1.000,V3C,Z17)
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & -1.000,V3C,Z17)
       deallocate(Z17)
       deallocate(X17)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S437),size(S437),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S437,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S438(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S438)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X13),size(X13),'3124',-1.000,
     & X13,S438)
       deallocate(S438)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,N2,M2,N0,M1,X13,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N2-N2,N1-N1,N0-N0,M1-M1/),'521346',t3B1,F2)
       allocate(Z13(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,X13,F2,Z13)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & -1.000,V3C,Z13)
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & 1.000,V3C,Z13)
       deallocate(Z13)
       deallocate(X13)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M2-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S435(N0+1:M1,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S435)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S435),size(S435),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S435,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S442(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S442)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X16),size(X16),'3124',-1.000,
     & X16,S442)
       deallocate(S442)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,N0,M1,X16,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N2-N2,N1-N1,N0-N0,M1-M1/),'521346',t3B1,F2)
       allocate(Z16(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X16,F2,Z16)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & 1.000,V3C,Z16)
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & -1.000,V3C,Z16)
       deallocate(Z16)
       deallocate(X16)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S435),size(S435),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S435,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S436(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S436)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'3124',-1.000,
     & X12,S436)
       deallocate(S436)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,N0,M1,X12,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N2-N2,N1-N1,N0-N0,M1-M1/),'521346',t3B1,F2)
       allocate(Z12(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X12,F2,Z12)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & -1.000,V3C,Z12)
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & 1.000,V3C,Z12)
       deallocate(Z12)
       deallocate(X12)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S439(N0+1:M1,N0+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S439)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S439),size(S439),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,N0-N0/),'2341',S439,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S444(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S444)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X18),size(X18),'3124',-1.000,
     & X18,S444)
       deallocate(S444)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,N0,M1,X18,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N2-N2,N1-N1,N0-N0,M1-M1/),'521346',t3B1,F2)
       allocate(Z18(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X18,F2,Z18)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & 1.000,V3C,Z18)
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & -1.000,V3C,Z18)
       deallocate(Z18)
       deallocate(X18)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S439),size(S439),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,N0-N0/),'2341',S439,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S440(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S440)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X14),size(X14),'3124',-1.000,
     & X14,S440)
       deallocate(S440)
C
       call sumx_sorted4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,N0,M1,X14,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N2-N2,N1-N1,N0-N0,M1-M1/),'521346',t3B1,F2)
       allocate(Z14(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X14,F2,Z14)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & -1.000,V3C,Z14)
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & 1.000,V3C,Z14)
       deallocate(Z14)
       deallocate(X14)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'2341',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S445(N0+1:M1,N0+1:M1,N0+1:M1,N2+1:N3))
       I1=K4*K5*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S445)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S445),size(S445),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N0-N0,N0-N0/),'4231',S445,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S446(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S446)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X49),size(X49),'3124',1.000,
     & X49,S446)
       deallocate(S446)
C
       call sumx_sorted3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,N0,M1,N0,M1,N0,M1,X49,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2,N1-N1,M1-M1/),'451236',t3C4,F2)
       allocate(Z49(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K0*K0
       I3=K5*K5
       call EGEMM(I1,I2,I3,X49,F2,Z49)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123645',
     & 0.500,V3C,Z49)
       deallocate(Z49)
       deallocate(X49)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N2-N2/),'2341',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S447(N0+1:M1,N0+1:M1,M1+1:N2,N2+1:N3))
       I1=K4*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S447)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S447),size(S447),shape(D1),size(D1),
     & (/N2-N2,N0-N0,M1-M1,N0-N0/),'4231',S447,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S448(N0+1:M1,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S448)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X50),size(X50),'3124',1.000,
     & X50,S448)
       deallocate(S448)
C
       call sumx_sorted3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,N0,M1,N0,M1,X50,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(Z50(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K0*K0
       I3=K8*K5
       call EGEMM(I1,I2,I3,X50,F2,Z50)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123645',
     & 1.000,V3C,Z50)
       deallocate(Z50)
       deallocate(X50)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,M1-N0,M1-N0,N2-N2/),'2341',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S449(N0+1:M1,M1+1:N2,M1+1:N2,N2+1:N3))
       I1=K4*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S449)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S449),size(S449),shape(D1),size(D1),
     & (/N2-N2,M1-M1,M1-M1,N0-N0/),'4231',S449,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S450(N0+1:M1,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S450)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X51),size(X51),'3124',1.000,
     & X51,S450)
       deallocate(S450)
C
       call sumx_sorted3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,N0,M1,N0,M1,X51,VCHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,N2-N2,N1-N1,M1-N0/),'451236',t3C1,F2)
       allocate(Z51(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K0*K0
       I3=K8*K8
       call EGEMM(I1,I2,I3,X51,F2,Z51)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123645',
     & 0.500,V3C,Z51)
       deallocate(Z51)
       deallocate(X51)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S451(N0+1:M1,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S451)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S451),size(S451),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S451,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S459(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S459)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X56),size(X56),'3124',1.000,
     & X56,S459)
       deallocate(S459)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,M1,X56,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'412356',t3C4,F2)
       allocate(Z56(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X56,F2,Z56)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & -1.000,V3C,Z56)
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & 1.000,V3C,Z56)
       deallocate(Z56)
       deallocate(X56)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S451),size(S451),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S451,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S452(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S452)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X52),size(X52),'3124',1.000,
     & X52,S452)
       deallocate(S452)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,M1,X52,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'412356',t3C4,F2)
       allocate(Z52(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X52,F2,Z52)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & 1.000,V3C,Z52)
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & -1.000,V3C,Z52)
       deallocate(Z52)
       deallocate(X52)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S455(N0+1:M1,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S455)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S455),size(S455),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'2341',S455,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S461(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S461)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X58),size(X58),'3124',1.000,
     & X58,S461)
       deallocate(S461)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,M1,X58,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'412356',t3C4,F2)
       allocate(Z58(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,X58,F2,Z58)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & -1.000,V3C,Z58)
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & 1.000,V3C,Z58)
       deallocate(Z58)
       deallocate(X58)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S455),size(S455),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'2341',S455,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S456(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S456)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X54),size(X54),'3124',1.000,
     & X54,S456)
       deallocate(S456)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,M1,X54,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'412356',t3C4,F2)
       allocate(Z54(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,X54,F2,Z54)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & 1.000,V3C,Z54)
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & -1.000,V3C,Z54)
       deallocate(Z54)
       deallocate(X54)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,M2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S453(N0+1:M1,N0+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S453)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S453),size(S453),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S453,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S460(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S460)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X57),size(X57),'3124',1.000,
     & X57,S460)
       deallocate(S460)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,M1,X57,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N1-N1,N0-N0,M1-N0/),'512346',t3C1,F2)
       allocate(Z57(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X57,F2,Z57)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & 1.000,V3C,Z57)
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & -1.000,V3C,Z57)
       deallocate(Z57)
       deallocate(X57)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S453),size(S453),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S453,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S454(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S454)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X53),size(X53),'3124',1.000,
     & X53,S454)
       deallocate(S454)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,M1,X53,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N1-N1,N0-N0,M1-N0/),'512346',t3C1,F2)
       allocate(Z53(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X53,F2,Z53)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & -1.000,V3C,Z53)
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & 1.000,V3C,Z53)
       deallocate(Z53)
       deallocate(X53)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,M1-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S457(N0+1:M1,N0+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S457)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S457),size(S457),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,N0-N0/),'2341',S457,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S462(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S462)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X59),size(X59),'3124',1.000,
     & X59,S462)
       deallocate(S462)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,M1,X59,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N1-N1,N0-N0,M1-N0/),'512346',t3C1,F2)
       allocate(Z59(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,X59,F2,Z59)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134625',
     & 1.000,V3C,Z59)
       call sum_stripe(6,shape(V3C),size(V3C),'135624',
     & -1.000,V3C,Z59)
       deallocate(Z59)
       deallocate(X59)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S457),size(S457),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,N0-N0/),'2341',S457,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S458(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S458)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X55),size(X55),'3124',1.000,
     & X55,S458)
       deallocate(S458)
C
       call sumx_sorted2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,M1,X55,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N1-N1,N0-N0,M1-N0/),'512346',t3C1,F2)
       allocate(Z55(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K7*K5*K9*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,X55,F2,Z55)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234615',
     & -1.000,V3C,Z55)
       call sum_stripe(6,shape(V3C),size(V3C),'235614',
     & 1.000,V3C,Z55)
       deallocate(Z55)
       deallocate(X55)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q68(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q68)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q68),size(Q68),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q68,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q69(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q69)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X22),size(X22),'21',1.000,
     & X22,Q69)
       deallocate(Q69)
C
       call sumx_sorted21(N0,N2,N0,N2,
     & N0,M1,N0,M1,X22,FBHH, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N2-N2,N1-N1,N0-N0,M1-M1/),'412356',t3C4,F2)
       allocate(Z22(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K5*K9*K0*K0
       I3=K5
       call EGEMM(I1,I2,I3,X22,F2,Z22)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123465',
     & 1.000,V3C,Z22)
       call sum_stripe(6,shape(V3C),size(V3C),'123564',
     & -1.000,V3C,Z22)
       deallocate(Z22)
       deallocate(X22)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q70(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q70)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder_shift(2,shape(Q70),size(Q70),shape(B1),size(B1),
     & (/N2-N2,M1-M1/),'21',Q70,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(Q71(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q71)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X23),size(X23),'21',1.000,
     & X23,Q71)
       deallocate(Q71)
C
       call sumx_sorted21(N0,N2,N0,N2,
     & M1,N2,N0,M1,X23,FBHH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C1),size(t3C1),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N1-N1,N0-N0,M1-N0/),'512346',t3C1,F2)
       allocate(Z23(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K5*K9*K0*K0
       I3=K8
       call EGEMM(I1,I2,I3,X23,F2,Z23)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'123465',
     & -1.000,V3C,Z23)
       call sum_stripe(6,shape(V3C),size(V3C),'123564',
     & 1.000,V3C,Z23)
       deallocate(Z23)
       deallocate(X23)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,M2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S463(N2+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S463)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(S463),size(S463),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N2-N2/),'2341',S463,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S464(N2+1:M2,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S464)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X60),size(X60),'3124',1.000,
     & X60,S464)
       deallocate(S464)
C
       call sumx_sorted1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,M2,N3,N2,M2,N2,M2,X60,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C2),size(t3C2),shape(F2),size(F2),
     & (/M2-M2,M2-M2,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3C2,F2)
       allocate(Z60(N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K7*K5*K5*K9
       I3=K6*K6
       call EGEMM(I1,I2,I3,X60,F2,Z60)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'345612',
     & 0.500,V3C,Z60)
       deallocate(Z60)
       deallocate(X60)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S465(N2+1:M2,N0+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S465)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S465),size(S465),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N2-N2,N2-N2/),'2341',S465,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S466(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S466)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X61),size(X61),'3124',1.000,
     & X61,S466)
       deallocate(S466)
C
       call sumx_sorted1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,N2,M2,N2,M2,N2,M2,X61,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3C4,F2)
       allocate(Z61(N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K7*K5*K5*K9
       I3=K0*K6
       call EGEMM(I1,I2,I3,X61,F2,Z61)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'345612',
     & 1.000,V3C,Z61)
       deallocate(Z61)
       deallocate(X61)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'4312',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S467(N2+1:M2,N0+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S467)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(S467),size(S467),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N2-N2/),'2341',S467,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S468(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S468)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X62),size(X62),'3124',1.000,
     & X62,S468)
       deallocate(S468)
C
       call sumx_sorted1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,M2,N2,M2,N2,M2,N2,M2,X62,VCAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3C4,F2)
       allocate(Z62(N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K7*K5*K5*K9
       I3=K0*K0
       call EGEMM(I1,I2,I3,X62,F2,Z62)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'345612',
     & 0.500,V3C,Z62)
       deallocate(Z62)
       deallocate(X62)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q72(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q72)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q76(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q72,B2,Q76)
       deallocate(B2)
C
       call sum_stripe(2,shape(X26),size(X26),'21',-1.000,
     & X26,Q76)
       deallocate(Q76)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & M2,N3,N2,M2,X26,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3C4,F2)
       allocate(Z26(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K5*K5*K9*K0
       I3=K6
       call EGEMM(I1,I2,I3,X26,F2,Z26)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134562',
     & -1.000,V3C,Z26)
       deallocate(Z26)
       deallocate(X26)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q73(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q72,B2,Q73)
       deallocate(B2)
C
       call sum_stripe(2,shape(X24),size(X24),'21',-1.000,
     & X24,Q73)
       deallocate(Q73)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & M2,N3,N2,M2,X24,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/M2-N2,N2-N2,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3C4,F2)
       allocate(Z24(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K5*K5*K9*K0
       I3=K6
       call EGEMM(I1,I2,I3,X24,F2,Z24)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234561',
     & 1.000,V3C,Z24)
       deallocate(Z24)
       deallocate(X24)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q74(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q74)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q77(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q74,B2,Q77)
       deallocate(B2)
C
       call sum_stripe(2,shape(X27),size(X27),'21',-1.000,
     & X27,Q77)
       deallocate(Q77)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & N2,M2,N2,M2,X27,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3C4,F2)
       allocate(Z27(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K5*K5*K9*K0
       I3=K0
       call EGEMM(I1,I2,I3,X27,F2,Z27)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134562',
     & -1.000,V3C,Z27)
       deallocate(Z27)
       deallocate(X27)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q75(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q74,B2,Q75)
       deallocate(B2)
C
       call sum_stripe(2,shape(X25),size(X25),'21',-1.000,
     & X25,Q75)
       deallocate(Q75)
C
       call sumx_sorted12(N2,N3,N2,N3,
     & N2,M2,N2,M2,X25,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3C4),size(t3C4),shape(F2),size(F2),
     & (/N2-N2,N2-N2,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3C4,F2)
       allocate(Z25(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K5*K5*K9*K0
       I3=K0
       call EGEMM(I1,I2,I3,X25,F2,Z25)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234561',
     & 1.000,V3C,Z25)
       deallocate(Z25)
       deallocate(X25)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N1-N1/),'1342',VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S469(N0+1:M1,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S469)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S469),size(S469),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S469,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S474(N2+1:M2,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S474)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2314',1.000,X4,
     & S474)
       deallocate(S474)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S469),size(S469),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S469,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(S475(N2+1:M2,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S475)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',1.000,X3,
     & S475)
       deallocate(S475)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S469),size(S469),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'3421',S469,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S476(N2+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S476)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X78),size(X78),'2314',1.000,
     & X78,S476)
       deallocate(S476)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S469),size(S469),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'3421',S469,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S478(N2+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S478)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X77),size(X77),'2314',1.000,
     & X77,S478)
       deallocate(S478)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S469),size(S469),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'3241',S469,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S516(N1+1:M2,N0+1:N2,N1+1:N3,N0+1:M1))
       I1=K5*K3*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S516)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S516),size(S516),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2314',S516,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S517(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S517)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',1.000,X3,
     & S517)
       deallocate(S517)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S469),size(S469),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'3421',S469,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S470(N1+1:M2,M1+1:N1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S470)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2413',1.000,X8,
     & S470)
       deallocate(S470)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S516),size(S516),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2314',S516,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S518(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S518)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',1.000,X4,
     & S518)
       deallocate(S518)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S471(N1+1:M2,M1+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S471)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S471),size(S471),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'3412',S471,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S472(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S472)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',-1.000,
     & X9,S472)
       deallocate(S472)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S471),size(S471),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'3412',S471,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S473(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S473)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',-1.000,
     & X10,S473)
       deallocate(S473)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'4132',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(S480(N1+1:M2,N0+1:M1,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K5*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S480)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S480),size(S480),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3412',S480,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S485(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S485)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',1.000,X4,
     & S485)
       deallocate(S485)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S480),size(S480),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3412',S480,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S481(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S481)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',1.000,X3,
     & S481)
       deallocate(S481)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N2-N2,N1-N1,N0-N0,N0-N0/),'1234',VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,M1-N0/),'1234',t2B,D2)
       allocate(S408(N0+1:M1,M1+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K7*K5
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S408)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S408),size(S408),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3412',S408,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S489(N2+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S489)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S489)
       deallocate(S489)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S408),size(S408),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'4312',S408,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S409(N1+1:M2,N0+1:N2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S409)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',-1.000,
     & X8,S409)
       deallocate(S409)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S408),size(S408),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3412',S408,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S484(N2+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S484)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S484)
       deallocate(S484)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S487(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S487)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S487),size(S487),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S487,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S488(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S488)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'3124',1.000,X7,
     & S488)
       deallocate(S488)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S482(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S482)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S482),size(S482),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S482,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S483(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S483)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',-1.000,
     & X7,S483)
       deallocate(S483)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q78(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q78)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q78),size(Q78),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q78,B1)
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,N0-N0,M1-N0/),'2134',t2B,D2)
       allocate(S486(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S486)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S486)
       deallocate(S486)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q78),size(Q78),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q78,B1)
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,N0-N0,M1-N0/),'2134',t2B,D2)
       allocate(S490(N2+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K0
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S490)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S490)
       deallocate(S490)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'1342',VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S491(N0+1:M1,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S491)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S491),size(S491),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S491,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S494(N2+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S494)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X77),size(X77),'2314',-1.000,
     & X77,S494)
       deallocate(S494)
C
       allocate(D2(N0+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(Z124(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K7*K9*K0
       I3=K2
       call EGEMM(I1,I2,I3,X77,D2,Z124)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'236145',
     & -1.000,V3C,Z124)
       call sum_stripe(6,shape(V3C),size(V3C),'236154',
     & 1.000,V3C,Z124)
       deallocate(Z124)
       deallocate(X77)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S491),size(S491),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'3421',S491,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S495(N1+1:M2,M1+1:N1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S495)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2413',1.000,X8,
     & S495)
       deallocate(S495)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S491),size(S491),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'2341',S491,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(S492(N2+1:M2,N2+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S492)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2314',-0.500,
     & X7,S492)
       deallocate(S492)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S491),size(S491),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N0-N0,N0-N0/),'4231',S491,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S519(N0+1:M1,N0+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S519)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S519),size(S519),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S519,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S520(N2+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S520)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',1.000,X5,
     & S520)
       deallocate(S520)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S491),size(S491),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'3241',S491,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S522(N2+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S522)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S522),size(S522),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'2314',S522,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S523(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S523)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',-1.000,
     & X7,S523)
       deallocate(S523)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder_shift(4,shape(S491),size(S491),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S491,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S493(N2+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S493)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X78),size(X78),'2314',-1.000,
     & X78,S493)
       deallocate(S493)
C
       allocate(D2(N0+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(Z125(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K7*K9*K0
       I3=K2
       call EGEMM(I1,I2,I3,X78,D2,Z125)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'136245',
     & 1.000,V3C,Z125)
       call sum_stripe(6,shape(V3C),size(V3C),'136254',
     & -1.000,V3C,Z125)
       deallocate(Z125)
       deallocate(X78)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S519),size(S519),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S519,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S521(N2+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S521)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'2134',1.000,X6,
     & S521)
       deallocate(S521)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S503(N1+1:M2,M1+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S503)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S503),size(S503),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'3412',S503,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S507(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S507)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',-1.000,
     & X10,S507)
       deallocate(S507)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S503),size(S503),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,M1-M1/),'3412',S503,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S504(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S504)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',-1.000,
     & X9,S504)
       deallocate(S504)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q79(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q79)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q79),size(Q79),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q79,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S508(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S508)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S508)
       deallocate(S508)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q79),size(Q79),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q79,B1)
       allocate(D2(N2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,M1-N0/),'1234',t2B,D2)
       allocate(S500(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K5*K9
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S500)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',1.000,X8,
     & S500)
       deallocate(S500)
C
       call sumx_sorted1234(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,M2,N0,M1,M1,N1,X8,VBHHPH, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(Z8(N2+1:M2,N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K9
       I2=K5*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X8,D2,Z8)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124356',
     & 1.000,V3C,Z8)
       call sum_stripe(6,shape(V3C),size(V3C),'125346',
     & -1.000,V3C,Z8)
       deallocate(Z8)
       deallocate(X8)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q79),size(Q79),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q79,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S506(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S506)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S506)
       deallocate(S506)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S404(N2+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S404)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S404),size(S404),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3412',S404,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S405(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S405)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'3124',-1.000,
     & X4,S405)
       deallocate(S405)
C
       call sumx_sorted2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N2,M2,N1,M2,N0,M1,X4,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,N0-N0,M1-N0/),'2134',t2B,D2)
       allocate(Z4(N2+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K0
       I2=K7*K5*K0
       I3=K3
       call EGEMM(I1,I2,I3,X4,D2,Z4)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'146235',
     & 1.000,V3C,Z4)
       call sum_stripe(6,shape(V3C),size(V3C),'156234',
     & -1.000,V3C,Z4)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S406(N2+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S406)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S406),size(S406),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'3412',S406,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S407(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S407)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'3124',-1.000,
     & X3,S407)
       deallocate(S407)
C
       call sumx_sorted2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N2,M2,N1,M2,N0,M1,X3,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N1-N1,N2-N2,N0-N0,M1-N0/),'2134',t2B,D2)
       allocate(Z3(N2+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K0
       I2=K7*K5*K0
       I3=K3
       call EGEMM(I1,I2,I3,X3,D2,Z3)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'246135',
     & -1.000,V3C,Z3)
       call sum_stripe(6,shape(V3C),size(V3C),'256134',
     & 1.000,V3C,Z3)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4132',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S496(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S496)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S496),size(S496),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S496,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S497(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S497)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',1.000,X7,
     & S497)
       deallocate(S497)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S414(N2+1:M2,M1+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S414)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S414),size(S414),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,M1-M1/),'4312',S414,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S479(N0+1:M1,N0+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K1
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S479)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'3124',-1.000,
     & X2,S479)
       deallocate(S479)
C
       call sumx_sorted2134(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,M2,N0,M1,M1,N1,X2,VBHHHP, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(Z2(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K0
       I2=K5*K9*K0
       I3=K1
       call EGEMM(I1,I2,I3,X2,D2,Z2)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'134256',
     & -1.000,V3C,Z2)
       call sum_stripe(6,shape(V3C),size(V3C),'135246',
     & 1.000,V3C,Z2)
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S414),size(S414),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S414,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S415(N1+1:M2,N2+1:N3,N2+1:M2,M1+1:N1))
       I1=K7*K0*K4
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S415)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'3124',1.000,
     & X10,S415)
       deallocate(S415)
C
       call sumx_sorted2314(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N2,M2,N1,M2,M1,N1,X10,VBHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(Z10(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       I1=K7*K9*K0
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X10,D2,Z10)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'145236',
     & -1.000,V3C,Z10)
       deallocate(Z10)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'3241',VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,M1-N0/),'3214',t2B,D2)
       allocate(S411(N2+1:M2,M1+1:N1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S411)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S411),size(S411),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,M1-M1/),'4312',S411,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N2-N2,N0-N0/),'12',t1B,B2)
       allocate(S477(N0+1:M1,N0+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K1
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S477)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'3124',-1.000,
     & X1,S477)
       deallocate(S477)
C
       call sumx_sorted2134(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,M2,N0,M1,M1,N1,X1,VBHHHP, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(Z1(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K0
       I2=K5*K9*K0
       I3=K1
       call EGEMM(I1,I2,I3,X1,D2,Z1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'234156',
     & 1.000,V3C,Z1)
       call sum_stripe(6,shape(V3C),size(V3C),'235146',
     & -1.000,V3C,Z1)
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S411),size(S411),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,M1-M1/),'3412',S411,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S412(N1+1:M2,N2+1:N3,N2+1:M2,M1+1:N1))
       I1=K7*K0*K4
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S412)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'3124',1.000,X9,
     & S412)
       deallocate(S412)
C
       call sumx_sorted2314(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N2,M2,N1,M2,M1,N1,X9,VBHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(Z9(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       I1=K7*K9*K0
       I2=K5*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X9,D2,Z9)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'245136',
     & 1.000,V3C,Z9)
       deallocate(Z9)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S498(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S498)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S498),size(S498),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S498,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S499(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S499)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'3124',1.000,X7,
     & S499)
       deallocate(S499)
C
       call sumx_sorted2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,M2,N2,M2,N0,M1,X7,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N2-N2,N1-N1,N0-N0,M1-N0/),'1234',t2B,D2)
       allocate(Z7(N1+1:M2,N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K0
       I2=K7*K5*K9
       I3=K4
       call EGEMM(I1,I2,I3,X7,D2,Z7)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'346125',
     & 1.000,V3C,Z7)
       call sum_stripe(6,shape(V3C),size(V3C),'356124',
     & -1.000,V3C,Z7)
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N2-N2,N2-N2,N0-N0,N0-N0/),'1234',t2C,D2)
       allocate(S501(N0+1:M1,N0+1:M1,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K5*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S501)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S501),size(S501),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3412',S501,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S502(N2+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S502)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',-0.500,
     & X5,S502)
       deallocate(S502)
C
       call sumx_sorted2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,M2,N0,M1,N0,M1,X5,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(Z5(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K7*K9*K0
       I3=K2
       call EGEMM(I1,I2,I3,X5,D2,Z5)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'236145',
     & -1.000,V3C,Z5)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S501),size(S501),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3412',S501,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S505(N2+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S505)
       deallocate(D1)
       deallocate(B2)
       deallocate(S501)
C
       call sum_stripe(4,shape(X6),size(X6),'2134',-0.500,
     & X6,S505)
       deallocate(S505)
C
       call sumx_sorted2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,M2,N0,M1,N0,M1,X6,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(Z6(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K7*K9*K0
       I3=K2
       call EGEMM(I1,I2,I3,X6,D2,Z6)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'136245',
     & 1.000,V3C,Z6)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,M1-N0/),'1324',VBHPPH,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z63(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z63)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z63)
       deallocate(Z63)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,M1-N0/),'1324',VBHPPH,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3D),size(t3D),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N2-N2,N2-N2,N0-N0,N0-N0/),'612345',t3D,F2)
       allocate(Z64(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z64)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3C),size(V3C),'124536',
     & 1.000,V3C,Z64)
       deallocate(Z64)
C
       call sumx_sorted3(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N2,M2,N2,M2,N1,M2,N0,M1,N0,M1,M1,N1,HT3C4,V3C,1.0)
       deallocate(V3C)
C
       end
