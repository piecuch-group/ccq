       subroutine t3A100110_update(N0,N1,N2,N3,HT3A,shift,
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
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::Q10(:,:)
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
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
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
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::S84(:,:,:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::S86(:,:,:,:)
       real*8,allocatable::S87(:,:,:,:)
       real*8,allocatable::S88(:,:,:,:)
       real*8,allocatable::S89(:,:,:,:)
       real*8,allocatable::S90(:,:,:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::S91(:,:,:,:)
       real*8,allocatable::S92(:,:,:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q18(:,:)
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
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::Q25(:,:)
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
       real*8,allocatable::Q28(:,:)
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
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::S266(:,:,:,:)
       real*8,allocatable::S267(:,:,:,:)
       real*8,allocatable::S268(:,:,:,:)
       real*8,allocatable::S269(:,:,:,:)
       real*8,allocatable::S270(:,:,:,:)
       real*8,allocatable::S271(:,:,:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::S272(:,:,:,:)
       real*8,allocatable::S273(:,:,:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q36(:,:)
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
       real*8,allocatable::Q37(:,:)
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
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q44(:,:)
       real*8,allocatable::Q45(:,:)
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
       real*8,allocatable::S398(:,:,:,:)
       real*8,allocatable::S399(:,:,:,:)
       real*8,allocatable::S400(:,:,:,:)
       real*8,allocatable::S401(:,:,:,:)
       real*8,allocatable::S402(:,:,:,:)
       real*8,allocatable::S403(:,:,:,:)
       real*8,allocatable::S405(:,:,:,:)
       real*8,allocatable::S406(:,:,:,:)
       real*8,allocatable::S404(:,:,:,:)
       real*8,allocatable::S407(:,:,:,:)
       real*8,allocatable::S411(:,:,:,:)
       real*8,allocatable::S408(:,:,:,:)
       real*8,allocatable::S409(:,:,:,:)
       real*8,allocatable::S410(:,:,:,:)
       real*8,allocatable::S412(:,:,:,:)
       real*8,allocatable::S414(:,:,:,:)
       real*8,allocatable::S415(:,:,:,:)
       real*8,allocatable::S413(:,:,:,:)
       real*8,allocatable::S416(:,:,:,:)
       real*8,allocatable::S422(:,:,:,:)
       real*8,allocatable::S423(:,:,:,:)
       real*8,allocatable::S417(:,:,:,:)
       real*8,allocatable::S418(:,:,:,:)
       real*8,allocatable::S424(:,:,:,:)
       real*8,allocatable::S426(:,:,:,:)
       real*8,allocatable::S419(:,:,:,:)
       real*8,allocatable::S420(:,:,:,:)
       real*8,allocatable::S421(:,:,:,:)
       real*8,allocatable::S427(:,:,:,:)
       real*8,allocatable::S425(:,:,:,:)
       real*8,allocatable::S428(:,:,:,:)
       real*8,allocatable::S430(:,:,:,:)
       real*8,allocatable::S431(:,:,:,:)
       real*8,allocatable::S429(:,:,:,:)
       real*8,allocatable::S432(:,:,:,:)
       real*8,allocatable::S436(:,:,:,:)
       real*8,allocatable::S433(:,:,:,:)
       real*8,allocatable::S434(:,:,:,:)
       real*8,allocatable::S435(:,:,:,:)
       real*8,allocatable::S437(:,:,:,:)
       real*8,allocatable::S443(:,:,:,:)
       real*8,allocatable::S444(:,:,:,:)
       real*8,allocatable::S438(:,:,:,:)
       real*8,allocatable::S439(:,:,:,:)
       real*8,allocatable::S445(:,:,:,:)
       real*8,allocatable::S447(:,:,:,:)
       real*8,allocatable::S440(:,:,:,:)
       real*8,allocatable::S441(:,:,:,:)
       real*8,allocatable::S442(:,:,:,:)
       real*8,allocatable::S448(:,:,:,:)
       real*8,allocatable::S446(:,:,:,:)
       real*8,allocatable::S449(:,:,:,:)
       real*8,allocatable::S450(:,:,:,:)
       real*8,allocatable::S451(:,:,:,:)
       real*8,allocatable::S452(:,:,:,:)
       real*8,allocatable::S453(:,:,:,:)
       real*8,allocatable::S457(:,:,:,:)
       real*8,allocatable::S454(:,:,:,:)
       real*8,allocatable::S455(:,:,:,:)
       real*8,allocatable::S458(:,:,:,:)
       real*8,allocatable::S456(:,:,:,:)
       real*8,allocatable::S459(:,:,:,:)
       real*8,allocatable::S460(:,:,:,:)
       real*8,allocatable::S461(:,:,:,:)
       real*8,allocatable::S462(:,:,:,:)
       real*8,allocatable::Q46(:,:)
       real*8,allocatable::Q50(:,:)
       real*8,allocatable::Q47(:,:)
       real*8,allocatable::S465(:,:,:,:)
       real*8,allocatable::S466(:,:,:,:)
       real*8,allocatable::S467(:,:,:,:)
       real*8,allocatable::S468(:,:,:,:)
       real*8,allocatable::S469(:,:,:,:)
       real*8,allocatable::S477(:,:,:,:)
       real*8,allocatable::S470(:,:,:,:)
       real*8,allocatable::S473(:,:,:,:)
       real*8,allocatable::S479(:,:,:,:)
       real*8,allocatable::S474(:,:,:,:)
       real*8,allocatable::S471(:,:,:,:)
       real*8,allocatable::S478(:,:,:,:)
       real*8,allocatable::S472(:,:,:,:)
       real*8,allocatable::S475(:,:,:,:)
       real*8,allocatable::S480(:,:,:,:)
       real*8,allocatable::S476(:,:,:,:)
       real*8,allocatable::S481(:,:,:,:)
       real*8,allocatable::S482(:,:,:,:)
       real*8,allocatable::S483(:,:,:,:)
       real*8,allocatable::S484(:,:,:,:)
       real*8,allocatable::S485(:,:,:,:)
       real*8,allocatable::S486(:,:,:,:)
       real*8,allocatable::S487(:,:,:,:)
       real*8,allocatable::S488(:,:,:,:)
       real*8,allocatable::Q48(:,:)
       real*8,allocatable::Q49(:,:)
       real*8,allocatable::S463(:,:,:,:)
       real*8,allocatable::S464(:,:,:,:)
       real*8,allocatable::S489(:,:,:,:)
       real*8,allocatable::S490(:,:,:,:)
       real*8,allocatable::S491(:,:,:,:)
       real*8,allocatable::S501(:,:,:,:)
       real*8,allocatable::S492(:,:,:,:)
       real*8,allocatable::S495(:,:,:,:)
       real*8,allocatable::S496(:,:,:,:)
       real*8,allocatable::S497(:,:,:,:)
       real*8,allocatable::S498(:,:,:,:)
       real*8,allocatable::S499(:,:,:,:)
       real*8,allocatable::S500(:,:,:,:)
       real*8,allocatable::Q51(:,:)
       real*8,allocatable::Q55(:,:)
       real*8,allocatable::Q57(:,:)
       real*8,allocatable::Q52(:,:)
       real*8,allocatable::S493(:,:,:,:)
       real*8,allocatable::S494(:,:,:,:)
       real*8,allocatable::S502(:,:,:,:)
       real*8,allocatable::Q53(:,:)
       real*8,allocatable::Q54(:,:)
       real*8,allocatable::Q58(:,:)
       real*8,allocatable::Q56(:,:)
       real*8,allocatable::S503(:,:,:,:)
       real*8,allocatable::S507(:,:,:,:)
       real*8,allocatable::S509(:,:,:,:)
       real*8,allocatable::S504(:,:,:,:)
       real*8,allocatable::S505(:,:,:,:)
       real*8,allocatable::S506(:,:,:,:)
       real*8,allocatable::S510(:,:,:,:)
       real*8,allocatable::S508(:,:,:,:)
       real*8,allocatable::S511(:,:,:,:)
       real*8,allocatable::S519(:,:,:,:)
       real*8,allocatable::S523(:,:,:,:)
       real*8,allocatable::S512(:,:,:,:)
       real*8,allocatable::S517(:,:,:,:)
       real*8,allocatable::S522(:,:,:,:)
       real*8,allocatable::S526(:,:,:,:)
       real*8,allocatable::S518(:,:,:,:)
       real*8,allocatable::S515(:,:,:,:)
       real*8,allocatable::S516(:,:,:,:)
       real*8,allocatable::S525(:,:,:,:)
       real*8,allocatable::S521(:,:,:,:)
       real*8,allocatable::S513(:,:,:,:)
       real*8,allocatable::S514(:,:,:,:)
       real*8,allocatable::S520(:,:,:,:)
       real*8,allocatable::S524(:,:,:,:)
       real*8,allocatable::Q59(:,:)
       real*8,allocatable::Q63(:,:)
       real*8,allocatable::Q60(:,:)
       real*8,allocatable::Q61(:,:)
       real*8,allocatable::Q62(:,:)
       real*8,allocatable::Q64(:,:)
       real*8,allocatable::Q68(:,:)
       real*8,allocatable::Q70(:,:)
       real*8,allocatable::Q65(:,:)
       real*8,allocatable::Q66(:,:)
       real*8,allocatable::Q67(:,:)
       real*8,allocatable::Q71(:,:)
       real*8,allocatable::Q69(:,:)
       real*8,allocatable::S527(:,:,:,:)
       real*8,allocatable::S529(:,:,:,:)
       real*8,allocatable::S530(:,:,:,:)
       real*8,allocatable::S531(:,:,:,:)
       real*8,allocatable::S532(:,:,:,:)
       real*8,allocatable::S533(:,:,:,:)
       real*8,allocatable::S611(:,:,:,:)
       real*8,allocatable::S612(:,:,:,:)
       real*8,allocatable::S615(:,:,:,:)
       real*8,allocatable::S616(:,:,:,:)
       real*8,allocatable::S617(:,:,:,:)
       real*8,allocatable::S618(:,:,:,:)
       real*8,allocatable::S528(:,:,:,:)
       real*8,allocatable::S613(:,:,:,:)
       real*8,allocatable::S614(:,:,:,:)
       real*8,allocatable::S619(:,:,:,:)
       real*8,allocatable::S534(:,:,:,:)
       real*8,allocatable::S542(:,:,:,:)
       real*8,allocatable::S543(:,:,:,:)
       real*8,allocatable::S535(:,:,:,:)
       real*8,allocatable::S536(:,:,:,:)
       real*8,allocatable::S537(:,:,:,:)
       real*8,allocatable::S538(:,:,:,:)
       real*8,allocatable::S539(:,:,:,:)
       real*8,allocatable::S540(:,:,:,:)
       real*8,allocatable::S620(:,:,:,:)
       real*8,allocatable::S621(:,:,:,:)
       real*8,allocatable::S624(:,:,:,:)
       real*8,allocatable::S625(:,:,:,:)
       real*8,allocatable::S626(:,:,:,:)
       real*8,allocatable::S627(:,:,:,:)
       real*8,allocatable::S541(:,:,:,:)
       real*8,allocatable::S622(:,:,:,:)
       real*8,allocatable::S623(:,:,:,:)
       real*8,allocatable::S628(:,:,:,:)
       real*8,allocatable::S561(:,:,:,:)
       real*8,allocatable::S562(:,:,:,:)
       real*8,allocatable::S563(:,:,:,:)
       real*8,allocatable::S564(:,:,:,:)
       real*8,allocatable::S565(:,:,:,:)
       real*8,allocatable::S566(:,:,:,:)
       real*8,allocatable::S553(:,:,:,:)
       real*8,allocatable::S568(:,:,:,:)
       real*8,allocatable::S573(:,:,:,:)
       real*8,allocatable::S554(:,:,:,:)
       real*8,allocatable::S567(:,:,:,:)
       real*8,allocatable::S556(:,:,:,:)
       real*8,allocatable::S569(:,:,:,:)
       real*8,allocatable::S574(:,:,:,:)
       real*8,allocatable::S557(:,:,:,:)
       real*8,allocatable::S571(:,:,:,:)
       real*8,allocatable::Q72(:,:)
       real*8,allocatable::S552(:,:,:,:)
       real*8,allocatable::S570(:,:,:,:)
       real*8,allocatable::S572(:,:,:,:)
       real*8,allocatable::S558(:,:,:,:)
       real*8,allocatable::S560(:,:,:,:)
       real*8,allocatable::S575(:,:,:,:)
       real*8,allocatable::S580(:,:,:,:)
       real*8,allocatable::S594(:,:,:,:)
       real*8,allocatable::S595(:,:,:,:)
       real*8,allocatable::S596(:,:,:,:)
       real*8,allocatable::S581(:,:,:,:)
       real*8,allocatable::S582(:,:,:,:)
       real*8,allocatable::S583(:,:,:,:)
       real*8,allocatable::S592(:,:,:,:)
       real*8,allocatable::S602(:,:,:,:)
       real*8,allocatable::S604(:,:,:,:)
       real*8,allocatable::S593(:,:,:,:)
       real*8,allocatable::S584(:,:,:,:)
       real*8,allocatable::S585(:,:,:,:)
       real*8,allocatable::S588(:,:,:,:)
       real*8,allocatable::S599(:,:,:,:)
       real*8,allocatable::S600(:,:,:,:)
       real*8,allocatable::S590(:,:,:,:)
       real*8,allocatable::S603(:,:,:,:)
       real*8,allocatable::S591(:,:,:,:)
       real*8,allocatable::S601(:,:,:,:)
       real*8,allocatable::S586(:,:,:,:)
       real*8,allocatable::S589(:,:,:,:)
       real*8,allocatable::S587(:,:,:,:)
       real*8,allocatable::Q73(:,:)
       real*8,allocatable::S607(:,:,:,:)
       real*8,allocatable::S608(:,:,:,:)
       real*8,allocatable::S609(:,:,:,:)
       real*8,allocatable::S610(:,:,:,:)
       real*8,allocatable::S605(:,:,:,:)
       real*8,allocatable::S606(:,:,:,:)
       real*8,allocatable::S546(:,:,:,:)
       real*8,allocatable::S551(:,:,:,:)
       real*8,allocatable::S547(:,:,:,:)
       real*8,allocatable::S544(:,:,:,:)
       real*8,allocatable::S545(:,:,:,:)
       real*8,allocatable::S550(:,:,:,:)
       real*8,allocatable::S576(:,:,:,:)
       real*8,allocatable::S578(:,:,:,:)
       real*8,allocatable::S579(:,:,:,:)
       real*8,allocatable::S577(:,:,:,:)
       real*8,allocatable::S548(:,:,:,:)
       real*8,allocatable::S549(:,:,:,:)
       real*8,allocatable::S559(:,:,:,:)
       real*8,allocatable::S555(:,:,:,:)
       real*8,allocatable::S597(:,:,:,:)
       real*8,allocatable::S598(:,:,:,:)
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
       real*8,allocatable::X13(:,:)
       real*8,allocatable::Z13(:,:,:,:,:,:)
       real*8,allocatable::X14(:,:)
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
       real*8,allocatable::Z57(:,:,:,:,:,:)
       real*8,allocatable::Z58(:,:,:,:,:,:)
       real*8,allocatable::X57(:,:,:,:)
       real*8,allocatable::Z59(:,:,:,:,:,:)
       real*8,allocatable::X58(:,:,:,:)
       real*8,allocatable::Z60(:,:,:,:,:,:)
       real*8,allocatable::X59(:,:,:,:)
       real*8,allocatable::Z61(:,:,:,:,:,:)
       real*8,allocatable::X60(:,:,:,:)
       real*8,allocatable::Z62(:,:,:,:,:,:)
       real*8,allocatable::X61(:,:,:,:)
       real*8,allocatable::Z63(:,:,:,:,:,:)
       real*8,allocatable::X62(:,:,:,:)
       real*8,allocatable::Z64(:,:,:,:,:,:)
       real*8,allocatable::X63(:,:,:,:)
       real*8,allocatable::Z65(:,:,:,:,:,:)
       real*8,allocatable::X64(:,:,:,:)
       real*8,allocatable::Z66(:,:,:,:,:,:)
       real*8,allocatable::Z67(:,:,:,:,:,:)
       real*8,allocatable::Z68(:,:,:,:,:,:)
       real*8,allocatable::Z69(:,:,:,:,:,:)
       real*8,allocatable::Z70(:,:,:,:,:,:)
       real*8,allocatable::X65(:,:,:,:)
       real*8,allocatable::Z92(:,:,:,:,:,:)
       real*8,allocatable::X66(:,:,:,:)
       real*8,allocatable::Z93(:,:,:,:,:,:)
       real*8,allocatable::X67(:,:,:,:)
       real*8,allocatable::Z94(:,:,:,:,:,:)
       real*8,allocatable::X68(:,:,:,:)
       real*8,allocatable::Z119(:,:,:,:,:,:)
       real*8,allocatable::X69(:,:,:,:)
       real*8,allocatable::Z120(:,:,:,:,:,:)
       real*8,allocatable::Z130(:,:,:,:,:,:)
       real*8,allocatable::Z131(:,:,:,:,:,:)
       real*8,allocatable::Z132(:,:,:,:,:,:)
       real*8,allocatable::X70(:,:,:,:)
       real*8,allocatable::Z141(:,:,:,:,:,:)
       real*8,allocatable::X71(:,:,:,:)
       real*8,allocatable::Z142(:,:,:,:,:,:)
       real*8,allocatable::X72(:,:,:,:)
       real*8,allocatable::Z143(:,:,:,:,:,:)
       real*8,allocatable::X73(:,:,:,:)
       real*8,allocatable::Z144(:,:,:,:,:,:)
       real*8,allocatable::Z181(:,:,:,:,:,:)
       real*8,allocatable::Z182(:,:,:,:,:,:)
       real*8,allocatable::Z183(:,:,:,:,:,:)
       real*8,allocatable::Z184(:,:,:,:,:,:)
       real*8,allocatable::Z185(:,:,:,:,:,:)
       real*8,allocatable::Z186(:,:,:,:,:,:)
       real*8,allocatable::Z187(:,:,:,:,:,:)
       real*8,allocatable::Z188(:,:,:,:,:,:)
       real*8,allocatable::Z189(:,:,:,:,:,:)
       real*8,allocatable::Z190(:,:,:,:,:,:)
       real*8,allocatable::Z191(:,:,:,:,:,:)
       real*8,allocatable::Z192(:,:,:,:,:,:)
       real*8,allocatable::Z193(:,:,:,:,:,:)
       real*8,allocatable::Z194(:,:,:,:,:,:)
       real*8,allocatable::Z195(:,:,:,:,:,:)
       real*8,allocatable::Z196(:,:,:,:,:,:)
       real*8,allocatable::Z197(:,:,:,:,:,:)
       real*8,allocatable::Z198(:,:,:,:,:,:)
       real*8,allocatable::Z199(:,:,:,:,:,:)
       real*8,allocatable::Z200(:,:,:,:,:,:)
       real*8,allocatable::Z201(:,:,:,:,:,:)
       real*8,allocatable::Z202(:,:,:,:,:,:)
       real*8,allocatable::Z203(:,:,:,:,:,:)
       real*8,allocatable::Z204(:,:,:,:,:,:)
       real*8,allocatable::Z205(:,:,:,:,:,:)
       real*8,allocatable::Z206(:,:,:,:,:,:)
       real*8,allocatable::Z207(:,:,:,:,:,:)
       real*8,allocatable::Z208(:,:,:,:,:,:)
       real*8,allocatable::Z209(:,:,:,:,:,:)
       real*8,allocatable::Z210(:,:,:,:,:,:)
       real*8,allocatable::Z211(:,:,:,:,:,:)
       real*8,allocatable::Z212(:,:,:,:,:,:)
       real*8,allocatable::Z213(:,:,:,:,:,:)
       real*8,allocatable::Z214(:,:,:,:,:,:)
       real*8,allocatable::Z215(:,:,:,:,:,:)
       real*8,allocatable::Z216(:,:,:,:,:,:)
       real*8,allocatable::Z312(:,:,:,:,:,:)
       real*8,allocatable::Z313(:,:,:,:,:,:)
       real*8,allocatable::Z360(:,:,:,:,:,:)
       real*8,allocatable::Z361(:,:,:,:,:,:)
       real*8,allocatable::Z362(:,:,:,:,:,:)
       real*8,allocatable::Z363(:,:,:,:,:,:)
       real*8,allocatable::Z378(:,:,:,:,:,:)
       real*8,allocatable::Z379(:,:,:,:,:,:)
       real*8,allocatable::X74(:,:,:,:)
       real*8,allocatable::Z406(:,:,:,:,:,:)
       real*8,allocatable::X75(:,:,:,:)
       real*8,allocatable::Z407(:,:,:,:,:,:)
       real*8,allocatable::X76(:,:,:,:)
       real*8,allocatable::Z411(:,:,:,:,:,:)
       real*8,allocatable::X77(:,:,:,:)
       real*8,allocatable::Z412(:,:,:,:,:,:)
       real*8,allocatable::X78(:,:,:,:)
       real*8,allocatable::Z413(:,:,:,:,:,:)
       real*8,allocatable::X79(:,:,:,:)
       real*8,allocatable::Z414(:,:,:,:,:,:)
       real*8,allocatable::X80(:,:,:,:)
       real*8,allocatable::Z455(:,:,:,:,:,:)
       real*8,allocatable::X81(:,:,:,:)
       real*8,allocatable::Z456(:,:,:,:,:,:)
       real*8,allocatable::X82(:,:,:,:)
       real*8,allocatable::Z457(:,:,:,:,:,:)
       real*8,allocatable::X83(:,:,:,:)
       real*8,allocatable::Z458(:,:,:,:,:,:)
       real*8,allocatable::X84(:,:,:,:)
       real*8,allocatable::Z465(:,:,:,:,:,:)
       real*8,allocatable::X85(:,:,:,:)
       real*8,allocatable::Z466(:,:,:,:,:,:)
       real*8,allocatable::X86(:,:,:,:)
       real*8,allocatable::Z467(:,:,:,:,:,:)
       real*8,allocatable::X87(:,:,:,:)
       real*8,allocatable::Z468(:,:,:,:,:,:)
       real*8,allocatable::X88(:,:,:,:)
       real*8,allocatable::Z469(:,:,:,:,:,:)
       real*8,allocatable::X89(:,:,:,:)
       real*8,allocatable::Z470(:,:,:,:,:,:)
       real*8,allocatable::X90(:,:,:,:)
       real*8,allocatable::Z471(:,:,:,:,:,:)
       real*8,allocatable::X91(:,:,:,:)
       real*8,allocatable::Z472(:,:,:,:,:,:)
C
       allocate(V3A(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       V3A=0.0d0
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHH),size(VAHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,M1-N0/),'3412',VAHHHH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S1(M2+1:N3,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,M2+1:N3,N0+1:M1,M1+1:N1))
       X1=0.0d0
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S1)
       deallocate(S1)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHH),size(VAHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,M1-N0/),'3412',VAHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S2(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N1,N1+1:M2,N0+1:M1,M1+1:N1))
       X2=0.0d0
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S2)
       deallocate(S2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHH),size(VAHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,M1-N0/),'4312',VAHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S3(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N1,N1+1:M2,N0+1:M1,M1+1:N1))
       X3=0.0d0
       call sum_stripe(4,shape(X3),size(X3),'2134',1.000,X3,
     & S3)
       deallocate(S3)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N1,M1-N0/),'1432',VAHPHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S4(N0+1:M1,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'3124',1.000,X1,
     & S4)
       deallocate(S4)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,M1-N0/),'1432',VAHPHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S5(N0+1:M1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'3124',1.000,X2,
     & S5)
       deallocate(S5)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,M1-N0/),'1432',VAHPHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S6(N0+1:M1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'3124',1.000,X3,
     & S6)
       deallocate(S6)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N1,M1-N0/),'4132',VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S7(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N1+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       X4=0.0d0
       call sum_stripe(4,shape(X4),size(X4),'3124',-1.000,
     & X4,S7)
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
       allocate(X5(N1+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       X5=0.0d0
       call sum_stripe(4,shape(X5),size(X5),'3124',-1.000,
     & X5,S8)
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
       call sum_stripe(4,shape(X4),size(X4),'2134',1.000,X4,
     & S9)
       deallocate(S9)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M1-N0/),'4132',VAHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S10(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',1.000,X5,
     & S10)
       deallocate(S10)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M1-N0/),'4132',VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S11(N1+1:M2,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N1+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       X6=0.0d0
       call sum_stripe(4,shape(X6),size(X6),'3124',-1.000,
     & X6,S11)
       deallocate(S11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M1-N0/),'4132',VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S12(N1+1:M2,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'2134',1.000,X6,
     & S12)
       deallocate(S12)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N1,N0-N0/),'1432',VAHPHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S13(M1+1:N1,N0+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'4123',-1.000,
     & X1,S13)
       deallocate(S13)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N0-N0/),'1432',VAHPHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S14(M1+1:N1,N0+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'4123',-1.000,
     & X2,S14)
       deallocate(S14)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N0-N0/),'1432',VAHPHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S15(M1+1:N1,N0+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'4123',-1.000,
     & X3,S15)
       deallocate(S15)
C
       allocate(D1(N1+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAAPPP),size(VAAPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,M2-N1,N1-N1/),'1234',VAAPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S16(M1+1:N1,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'4123',-1.000,
     & X4,S16)
       deallocate(S16)
C
       allocate(D1(N1+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAAPPP),size(VAAPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,M2-N1,N1-N1/),'1234',VAAPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S17(M1+1:N1,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'4123',-1.000,
     & X5,S17)
       deallocate(S17)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAAPPP),size(VAAPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N1-N1,N1-N1/),'1234',VAAPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S18(M1+1:N1,N1+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K3
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'4123',-1.000,
     & X6,S18)
       deallocate(S18)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHH),size(VAHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,N0-N0/),'3412',VAHHHH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S19(M2+1:N3,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:N1,M2+1:N3,N0+1:M1,N0+1:M1))
       X7=0.0d0
       call sum_stripe(4,shape(X7),size(X7),'2134',-1.000,
     & X7,S19)
       deallocate(S19)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHH),size(VAHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,N0-N0/),'3412',VAHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S20(N1+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:N1,N1+1:M2,N0+1:M1,N0+1:M1))
       X8=0.0d0
       call sum_stripe(4,shape(X8),size(X8),'2134',-1.000,
     & X8,S20)
       deallocate(S20)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHH),size(VAHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,N0-N0/),'4312',VAHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S21(N1+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:N1,N1+1:M2,N0+1:M1,N0+1:M1))
       X9=0.0d0
       call sum_stripe(4,shape(X9),size(X9),'2134',1.000,X9,
     & S21)
       deallocate(S21)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N1,N0-N0/),'1432',VAHPHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S22(N0+1:M1,N0+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X65(N0+1:N1,M2+1:N3,N0+1:M1,N0+1:M1))
       X65=0.0d0
       call sum_stripe(4,shape(X65),size(X65),'3124',1.000,
     & X65,S22)
       deallocate(S22)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N0-N0/),'1432',VAHPHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S23(N0+1:M1,N0+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X66(N0+1:N1,N1+1:M2,N0+1:M1,N0+1:M1))
       X66=0.0d0
       call sum_stripe(4,shape(X66),size(X66),'3124',1.000,
     & X66,S23)
       deallocate(S23)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N0-N0/),'1432',VAHPHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S24(N0+1:M1,N0+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X67(N0+1:N1,N1+1:M2,N0+1:M1,N0+1:M1))
       X67=0.0d0
       call sum_stripe(4,shape(X67),size(X67),'3124',1.000,
     & X67,S24)
       deallocate(S24)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N1,N0-N0/),'4132',VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S25(N1+1:M2,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N1+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       X10=0.0d0
       call sum_stripe(4,shape(X10),size(X10),'3124',-1.000,
     & X10,S25)
       deallocate(S25)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N1,N0-N0/),'4132',VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S26(N1+1:M2,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N1+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       X11=0.0d0
       call sum_stripe(4,shape(X11),size(X11),'3124',-1.000,
     & X11,S26)
       deallocate(S26)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'4132',VAHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S27(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',1.000,
     & X10,S27)
       deallocate(S27)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'4132',VAHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S28(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'2134',1.000,
     & X11,S28)
       deallocate(S28)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'4132',VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S29(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(N1+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       X12=0.0d0
       call sum_stripe(4,shape(X12),size(X12),'3124',-1.000,
     & X12,S29)
       deallocate(S29)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'4132',VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S30(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'2134',1.000,
     & X12,S30)
       deallocate(S30)
C
       allocate(D1(N1+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAAPPP),size(VAAPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,M2-N1,N1-N1/),'1234',VAAPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S31(N0+1:M1,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'4123',-1.000,
     & X10,S31)
       deallocate(S31)
C
       allocate(D1(N1+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAAPPP),size(VAAPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,M2-N1,N1-N1/),'1234',VAAPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S32(N0+1:M1,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'4123',-1.000,
     & X11,S32)
       deallocate(S32)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAAPPP),size(VAAPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N1-N1,N1-N1/),'1234',VAAPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S33(N0+1:M1,N1+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K3
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'4123',-1.000,
     & X12,S33)
       deallocate(S33)
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
       allocate(X13(M1+1:N1,M1+1:N1))
       X13=0.0d0
       call sum_stripe(2,shape(X13),size(X13),'21',1.000,
     & X13,Q1)
       deallocate(Q1)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(Q2(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q2)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X14(N0+1:M1,N0+1:M1))
       X14=0.0d0
       call sum_stripe(2,shape(X14),size(X14),'21',1.000,
     & X14,Q2)
       deallocate(Q2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,M1-N0/),'12',FAHP,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(Q3(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q3)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X15(M1+1:N1,N0+1:M1))
       X15=0.0d0
       call sum_stripe(2,shape(X15),size(X15),'21',1.000,
     & X15,Q3)
       deallocate(Q3)
C
       allocate(B1(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,M2-N1/),'21',FAHP,B1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q4(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q4)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X16(M2+1:N3,M2+1:N3))
       X16=0.0d0
       call sum_stripe(2,shape(X16),size(X16),'21',-1.000,
     & X16,Q4)
       deallocate(Q4)
C
       allocate(B1(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,N1-N1/),'21',FAHP,B1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q5(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q5)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X17(N1+1:M2,M2+1:N3))
       X17=0.0d0
       call sum_stripe(2,shape(X17),size(X17),'21',-1.000,
     & X17,Q5)
       deallocate(Q5)
C
       allocate(B1(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,M2-N1/),'21',FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q6(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q6)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X18(M2+1:N3,N1+1:M2))
       X18=0.0d0
       call sum_stripe(2,shape(X18),size(X18),'21',-1.000,
     & X18,Q6)
       deallocate(Q6)
C
       allocate(B1(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,N1-N1/),'21',FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q7(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q7)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X19(N1+1:M2,N1+1:M2))
       X19=0.0d0
       call sum_stripe(2,shape(X19),size(X19),'21',-1.000,
     & X19,Q7)
       deallocate(Q7)
C
       allocate(B1(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,M2-N1/),'21',FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q8(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q8)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X20(M2+1:N3,N1+1:M2))
       X20=0.0d0
       call sum_stripe(2,shape(X20),size(X20),'21',-1.000,
     & X20,Q8)
       deallocate(Q8)
C
       allocate(B1(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,N1-N1/),'21',FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q9(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q9)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X21(N1+1:M2,N1+1:M2))
       X21=0.0d0
       call sum_stripe(2,shape(X21),size(X21),'21',-1.000,
     & X21,Q9)
       deallocate(Q9)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,M1-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S34(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X22(N0+1:M1,M1+1:N1,N0+1:M1,M1+1:N1))
       X22=0.0d0
       call sum_stripe(4,shape(X22),size(X22),'3124',1.000,
     & X22,S34)
       deallocate(S34)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,M1-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S35(N0+1:M1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X23(M1+1:N1,M1+1:N1,N0+1:M1,M1+1:N1))
       X23=0.0d0
       call sum_stripe(4,shape(X23),size(X23),'3124',1.000,
     & X23,S35)
       deallocate(S35)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S36(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X24(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       X24=0.0d0
       call sum_stripe(4,shape(X24),size(X24),'3124',-1.000,
     & X24,S36)
       deallocate(S36)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S37(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X25(M1+1:N1,N1+1:M2,M2+1:N3,M1+1:N1))
       X25=0.0d0
       call sum_stripe(4,shape(X25),size(X25),'3124',-1.000,
     & X25,S37)
       deallocate(S37)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S38(N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X26(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       X26=0.0d0
       call sum_stripe(4,shape(X26),size(X26),'3124',-1.000,
     & X26,S38)
       deallocate(S38)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S39(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X27(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X27=0.0d0
       call sum_stripe(4,shape(X27),size(X27),'3124',-1.000,
     & X27,S39)
       deallocate(S39)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,M1-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S40(N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X68(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       X68=0.0d0
       call sum_stripe(4,shape(X68),size(X68),'3124',1.000,
     & X68,S40)
       deallocate(S40)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,M1-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S41(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X69(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X69=0.0d0
       call sum_stripe(4,shape(X69),size(X69),'3124',1.000,
     & X69,S41)
       deallocate(S41)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M1-N0/),'3142',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q10(M1+1:N1,M1+1:N1))
       I1=K7*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       X13=X13+Q10
       deallocate(Q10)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N0-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S42(M1+1:N1,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X22),size(X22),'4123',-1.000,
     & X22,S42)
       deallocate(S42)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,N0-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S43(M1+1:N1,M1+1:N1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X23),size(X23),'4123',-1.000,
     & X23,S43)
       deallocate(S43)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N1,M2-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S44(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X24),size(X24),'4123',-1.000,
     & X24,S44)
       deallocate(S44)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N1-N1,M2-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S45(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X25),size(X25),'4123',-1.000,
     & X25,S45)
       deallocate(S45)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S46(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X26),size(X26),'4123',-1.000,
     & X26,S46)
       deallocate(S46)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N1-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S47(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X27),size(X27),'4123',-1.000,
     & X27,S47)
       deallocate(S47)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S48(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X28(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       X28=0.0d0
       call sum_stripe(4,shape(X28),size(X28),'4123',-1.000,
     & X28,S48)
       deallocate(S48)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N1-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S49(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X29(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X29=0.0d0
       call sum_stripe(4,shape(X29),size(X29),'4123',-1.000,
     & X29,S49)
       deallocate(S49)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N0-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S50(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S50),size(S50),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S50,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z130(M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K9*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z130)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'123645',
     & 0.500,V3A,Z130)
       call sum_stripe(6,shape(V3A),size(V3A),'123654',
     & -0.500,V3A,Z130)
       deallocate(Z130)
       deallocate(S50)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N0-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S51(N0+1:M1,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S51),size(S51),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N0-N0,N0-N0/),'2314',S51,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z131(M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K9*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z131)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'123645',
     & 1.000,V3A,Z131)
       call sum_stripe(6,shape(V3A),size(V3A),'123654',
     & -1.000,V3A,Z131)
       deallocate(Z131)
       deallocate(S51)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,N0-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S52(N0+1:M1,M1+1:N1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M1+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S52),size(S52),shape(D1),size(D1),
     & (/M1-M1,M1-M1,N0-N0,N0-N0/),'2314',S52,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z132(M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K9*K6
       I3=K7*K7
       call EGEMM(I1,I2,I3,D1,F2,Z132)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'123645',
     & 0.500,V3A,Z132)
       call sum_stripe(6,shape(V3A),size(V3A),'123654',
     & -0.500,V3A,Z132)
       deallocate(Z132)
       deallocate(S52)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N0-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S53(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X33(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X33=0.0d0
       call sum_stripe(4,shape(X33),size(X33),'3124',-1.000,
     & X33,S53)
       deallocate(S53)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N0-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S54(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X34(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       X34=0.0d0
       call sum_stripe(4,shape(X34),size(X34),'3124',-1.000,
     & X34,S54)
       deallocate(S54)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S55(M2+1:N3,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X35(N0+1:M1,N1+1:M2,M2+1:N3,N0+1:M1))
       X35=0.0d0
       call sum_stripe(4,shape(X35),size(X35),'3124',-1.000,
     & X35,S55)
       deallocate(S55)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N0-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S56(M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X36(M1+1:N1,N1+1:M2,M2+1:N3,N0+1:M1))
       X36=0.0d0
       call sum_stripe(4,shape(X36),size(X36),'3124',-1.000,
     & X36,S56)
       deallocate(S56)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N0-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S57(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X37(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X37=0.0d0
       call sum_stripe(4,shape(X37),size(X37),'3124',-1.000,
     & X37,S57)
       deallocate(S57)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N0-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S58(N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X38(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       X38=0.0d0
       call sum_stripe(4,shape(X38),size(X38),'3124',-1.000,
     & X38,S58)
       deallocate(S58)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S59(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X39(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X39=0.0d0
       call sum_stripe(4,shape(X39),size(X39),'3124',-1.000,
     & X39,S59)
       deallocate(S59)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N0-N0/),'3412',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S60(N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X40(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       X40=0.0d0
       call sum_stripe(4,shape(X40),size(X40),'3124',-1.000,
     & X40,S60)
       deallocate(S60)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N0-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S61(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X70(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X70=0.0d0
       call sum_stripe(4,shape(X70),size(X70),'3124',1.000,
     & X70,S61)
       deallocate(S61)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N0-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S62(N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S62)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X71(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       X71=0.0d0
       call sum_stripe(4,shape(X71),size(X71),'3124',1.000,
     & X71,S62)
       deallocate(S62)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S63(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X72(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X72=0.0d0
       call sum_stripe(4,shape(X72),size(X72),'3124',1.000,
     & X72,S63)
       deallocate(S63)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N0-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S64(N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X73(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       X73=0.0d0
       call sum_stripe(4,shape(X73),size(X73),'3124',1.000,
     & X73,S64)
       deallocate(S64)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'3142',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q11(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       X14=X14+Q11
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N0-N0/),'3142',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q12(M1+1:N1,N0+1:M1))
       I1=K5*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       X15=X15+Q12
       deallocate(Q12)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N1,M2-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S65(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X33),size(X33),'4123',-1.000,
     & X33,S65)
       deallocate(S65)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N1,M2-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S66(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S66)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X34),size(X34),'4123',-1.000,
     & X34,S66)
       deallocate(S66)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,M2-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S67(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X35),size(X35),'4123',-1.000,
     & X35,S67)
       deallocate(S67)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N1-N1,M2-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S68(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S68)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X36),size(X36),'4123',-1.000,
     & X36,S68)
       deallocate(S68)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S69(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X37),size(X37),'4123',-1.000,
     & X37,S69)
       deallocate(S69)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S70(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S70)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X38),size(X38),'4123',-1.000,
     & X38,S70)
       deallocate(S70)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S71(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X39),size(X39),'4123',-1.000,
     & X39,S71)
       deallocate(S71)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N1-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S72(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S72)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X40),size(X40),'4123',-1.000,
     & X40,S72)
       deallocate(S72)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S73(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X41(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X41=0.0d0
       call sum_stripe(4,shape(X41),size(X41),'4123',-1.000,
     & X41,S73)
       deallocate(S73)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S74(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S74)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X42(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       X42=0.0d0
       call sum_stripe(4,shape(X42),size(X42),'4123',-1.000,
     & X42,S74)
       deallocate(S74)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S75(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X43(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X43=0.0d0
       call sum_stripe(4,shape(X43),size(X43),'4123',-1.000,
     & X43,S75)
       deallocate(S75)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N1-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S76(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S76)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X44(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       X44=0.0d0
       call sum_stripe(4,shape(X44),size(X44),'4123',-1.000,
     & X44,S76)
       deallocate(S76)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,M2-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S77(N1+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       I1=K6*K6*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X45(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       X45=0.0d0
       call sum_stripe(4,shape(X45),size(X45),'4123',-1.000,
     & X45,S77)
       deallocate(S77)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S78(N1+1:M2,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S78)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X46(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       X46=0.0d0
       call sum_stripe(4,shape(X46),size(X46),'4123',-1.000,
     & X46,S78)
       deallocate(S78)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S79(N1+1:M2,N1+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X47(N1+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       X47=0.0d0
       call sum_stripe(4,shape(X47),size(X47),'4123',-1.000,
     & X47,S79)
       deallocate(S79)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,M2-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S80(N1+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       I1=K6*K6*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S80)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X48(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       X48=0.0d0
       call sum_stripe(4,shape(X48),size(X48),'4123',-1.000,
     & X48,S80)
       deallocate(S80)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S81(N1+1:M2,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S81)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X49(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       X49=0.0d0
       call sum_stripe(4,shape(X49),size(X49),'4123',-1.000,
     & X49,S81)
       deallocate(S81)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S82(N1+1:M2,N1+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S82)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X50(N1+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       X50=0.0d0
       call sum_stripe(4,shape(X50),size(X50),'4123',-1.000,
     & X50,S82)
       deallocate(S82)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N1,M2-N1/),'4123',VAHPPP,D1)
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
       X16=X16-Q13
       deallocate(Q13)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q14(N1+1:M2,M2+1:N3))
       I1=K6*K9
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       X17=X17-Q14
       deallocate(Q14)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,M2-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S83(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X45),size(X45),'3124',1.000,
     & X45,S83)
       deallocate(S83)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S84(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S84)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X46),size(X46),'3124',1.000,
     & X46,S84)
       deallocate(S84)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S85(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S85)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X47),size(X47),'3124',1.000,
     & X47,S85)
       deallocate(S85)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,M2-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S86(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S86)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X48),size(X48),'3124',1.000,
     & X48,S86)
       deallocate(S86)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S87(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S87)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X49),size(X49),'3124',1.000,
     & X49,S87)
       deallocate(S87)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S88(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S88)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X50),size(X50),'3124',1.000,
     & X50,S88)
       deallocate(S88)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S89(N1+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S89)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X51(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       X51=0.0d0
       call sum_stripe(4,shape(X51),size(X51),'4123',-1.000,
     & X51,S89)
       deallocate(S89)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S90(N1+1:M2,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X52(N1+1:M2,N1+1:M2,N1+1:M2,N1+1:M2))
       X52=0.0d0
       call sum_stripe(4,shape(X52),size(X52),'4123',-1.000,
     & X52,S90)
       deallocate(S90)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q15(M2+1:N3,N1+1:M2))
       I1=K9*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       X18=X18-Q15
       deallocate(Q15)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q16(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q16)
       deallocate(D1)
       deallocate(B2)
C
       X19=X19-Q16
       deallocate(Q16)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S91(N1+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X51),size(X51),'3124',1.000,
     & X51,S91)
       deallocate(S91)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S92(N1+1:M2,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S92)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X52),size(X52),'3124',1.000,
     & X52,S92)
       deallocate(S92)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M2-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q17(M2+1:N3,N1+1:M2))
       I1=K9*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q17)
       deallocate(D1)
       deallocate(B2)
C
       X20=X20-Q17
       deallocate(Q17)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q18(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q18)
       deallocate(D1)
       deallocate(B2)
C
       X21=X21-Q18
       deallocate(Q18)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S93(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S93),size(S93),shape(D1),size(D1),
     & (/M1-M1,M2-M2,M2-M2,M1-M1/),'2314',S93,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z181(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z181)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & -1.000,V3A,Z181)
       deallocate(Z181)
       deallocate(S93)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S94(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S94)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S94),size(S94),shape(D1),size(D1),
     & (/M1-M1,N2-N2,M2-M2,M1-M1/),'2314',S94,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z182(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z182)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & -1.000,V3A,Z182)
       deallocate(Z182)
       deallocate(S94)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S95(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S95),size(S95),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,M1-M1/),'2314',S95,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z183(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z183)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & 1.000,V3A,Z183)
       deallocate(Z183)
       deallocate(S95)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S96(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S96)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S96),size(S96),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,M1-M1/),'2314',S96,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z184(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z184)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & 1.000,V3A,Z184)
       deallocate(Z184)
       deallocate(S96)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S97(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S97),size(S97),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,M1-M1/),'2314',S97,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z185(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z185)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & -1.000,V3A,Z185)
       deallocate(Z185)
       deallocate(S97)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M1-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S98(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S98)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S98),size(S98),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,M1-M1/),'2314',S98,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z186(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z186)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & -1.000,V3A,Z186)
       deallocate(Z186)
       deallocate(S98)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N2,M2-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S99(M1+1:N1,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S99),size(S99),shape(D1),size(D1),
     & (/M1-M1,M2-M2,M2-M2,M1-M1/),'2341',S99,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z187(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z187)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z187)
       deallocate(Z187)
       deallocate(S99)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,M2-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S100(M1+1:N1,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S100)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S100),size(S100),shape(D1),size(D1),
     & (/M1-M1,N2-N2,M2-M2,M1-M1/),'2341',S100,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z188(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z188)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z188)
       deallocate(Z188)
       deallocate(S100)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S101(M1+1:N1,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S101),size(S101),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,M1-M1/),'2341',S101,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z189(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z189)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z189)
       deallocate(Z189)
       deallocate(S101)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S102(M1+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S102)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S102),size(S102),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,M1-M1/),'2341',S102,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z190(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z190)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z190)
       deallocate(Z190)
       deallocate(S102)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S103(M1+1:N1,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S103),size(S103),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,M1-M1/),'2341',S103,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z191(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z191)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z191)
       deallocate(Z191)
       deallocate(S103)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S104(M1+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S104)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S104),size(S104),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,M1-M1/),'2341',S104,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z192(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z192)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z192)
       deallocate(Z192)
       deallocate(S104)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S105(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S105),size(S105),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N0-N0/),'2314',S105,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z193(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z193)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & 1.000,V3A,Z193)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & -1.000,V3A,Z193)
       deallocate(Z193)
       deallocate(S105)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S106(M2+1:N3,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S106)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S106),size(S106),shape(D1),size(D1),
     & (/M1-M1,M2-M2,M2-M2,N0-N0/),'2314',S106,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z194(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z194)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & 1.000,V3A,Z194)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & -1.000,V3A,Z194)
       deallocate(Z194)
       deallocate(S106)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S107(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S107)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S107),size(S107),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,N0-N0/),'2314',S107,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z195(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z195)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & 1.000,V3A,Z195)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & -1.000,V3A,Z195)
       deallocate(Z195)
       deallocate(S107)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S108(M2+1:N3,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S108)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S108),size(S108),shape(D1),size(D1),
     & (/M1-M1,N2-N2,M2-M2,N0-N0/),'2314',S108,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z196(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z196)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & 1.000,V3A,Z196)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & -1.000,V3A,Z196)
       deallocate(Z196)
       deallocate(S108)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S109(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S109)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S109),size(S109),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N0-N0/),'2314',S109,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z197(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z197)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & -1.000,V3A,Z197)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & 1.000,V3A,Z197)
       deallocate(Z197)
       deallocate(S109)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S110(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S110)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S110),size(S110),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,N0-N0/),'2314',S110,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z198(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z198)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & -1.000,V3A,Z198)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & 1.000,V3A,Z198)
       deallocate(Z198)
       deallocate(S110)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S111(N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S111)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S111),size(S111),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'2314',S111,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z199(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z199)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & -1.000,V3A,Z199)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & 1.000,V3A,Z199)
       deallocate(Z199)
       deallocate(S111)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S112(N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S112)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S112),size(S112),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,N0-N0/),'2314',S112,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z200(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z200)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & -1.000,V3A,Z200)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & 1.000,V3A,Z200)
       deallocate(Z200)
       deallocate(S112)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S113(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S113)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S113),size(S113),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N0-N0/),'2314',S113,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z201(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z201)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z201)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z201)
       deallocate(Z201)
       deallocate(S113)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S114(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S114)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S114),size(S114),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,N0-N0/),'2314',S114,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z202(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z202)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z202)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z202)
       deallocate(Z202)
       deallocate(S114)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S115(N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S115)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S115),size(S115),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'2314',S115,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z203(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z203)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z203)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z203)
       deallocate(Z203)
       deallocate(S115)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N0-N0/),'4312',VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S116(N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S116)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S116),size(S116),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,N0-N0/),'2314',S116,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z204(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z204)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z204)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z204)
       deallocate(Z204)
       deallocate(S116)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N2,M2-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S117(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S117)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S117),size(S117),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N0-N0/),'2341',S117,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z205(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z205)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z205)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z205)
       deallocate(Z205)
       deallocate(S117)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N2,M2-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S118(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S118)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S118),size(S118),shape(D1),size(D1),
     & (/M1-M1,M2-M2,M2-M2,N0-N0/),'2341',S118,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z206(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z206)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z206)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z206)
       deallocate(Z206)
       deallocate(S118)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,M2-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S119(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S119)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S119),size(S119),shape(D1),size(D1),
     & (/N0-N0,N2-N2,M2-M2,N0-N0/),'2341',S119,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z207(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z207)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z207)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z207)
       deallocate(Z207)
       deallocate(S119)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,M2-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S120(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S120)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S120),size(S120),shape(D1),size(D1),
     & (/M1-M1,N2-N2,M2-M2,N0-N0/),'2341',S120,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z208(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z208)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z208)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z208)
       deallocate(Z208)
       deallocate(S120)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S121(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S121)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S121),size(S121),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N0-N0/),'2341',S121,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z209(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z209)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z209)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z209)
       deallocate(Z209)
       deallocate(S121)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S122(N0+1:M1,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S122)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S122),size(S122),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,N0-N0/),'2341',S122,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z210(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z210)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z210)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z210)
       deallocate(Z210)
       deallocate(S122)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S123(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S123)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S123),size(S123),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'2341',S123,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z211(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z211)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z211)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z211)
       deallocate(Z211)
       deallocate(S123)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S124(N0+1:M1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S124)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S124),size(S124),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,N0-N0/),'2341',S124,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z212(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z212)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z212)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z212)
       deallocate(Z212)
       deallocate(S124)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S125(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S125)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S125),size(S125),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N0-N0/),'2341',S125,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z213(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z213)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z213)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z213)
       deallocate(Z213)
       deallocate(S125)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S126(N0+1:M1,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S126)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S126),size(S126),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,N0-N0/),'2341',S126,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z214(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z214)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z214)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z214)
       deallocate(Z214)
       deallocate(S126)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S127(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S127)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S127),size(S127),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'2341',S127,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z215(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z215)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z215)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z215)
       deallocate(Z215)
       deallocate(S127)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S128(N0+1:M1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S128)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S128),size(S128),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,N0-N0/),'2341',S128,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z216(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z216)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z216)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z216)
       deallocate(Z216)
       deallocate(S128)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q19(M1+1:N1,M1+1:N1))
       I1=K7*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q19)
       deallocate(D1)
       deallocate(B2)
C
       X13=X13+Q19
       deallocate(Q19)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VBHHHP,D1)
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
       X14=X14+Q20
       deallocate(Q20)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N0-N0/),'3142',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q21(M1+1:N1,N0+1:M1))
       I1=K5*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q21)
       deallocate(D1)
       deallocate(B2)
C
       X15=X15+Q21
       deallocate(Q21)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,M2-N1/),'3124',VBPHPP,D1)
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
       X16=X16+Q22
       deallocate(Q22)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q23(N1+1:M2,M2+1:N3))
       I1=K6*K9
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q23)
       deallocate(D1)
       deallocate(B2)
C
       X17=X17+Q23
       deallocate(Q23)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q24(M2+1:N3,N1+1:M2))
       I1=K9*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q24)
       deallocate(D1)
       deallocate(B2)
C
       X18=X18+Q24
       deallocate(Q24)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q25(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q25)
       deallocate(D1)
       deallocate(B2)
C
       X19=X19+Q25
       deallocate(Q25)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q26(M2+1:N3,N1+1:M2))
       I1=K9*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q26)
       deallocate(D1)
       deallocate(B2)
C
       X20=X20+Q26
       deallocate(Q26)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q27(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q27)
       deallocate(D1)
       deallocate(B2)
C
       X21=X21+Q27
       deallocate(Q27)
C
       allocate(B1(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,N1-N1/),'21',FAHP,B1)
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S129(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,D2,S129)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',-1.000,
     & X12,S129)
       deallocate(S129)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',t2A,D2)
       allocate(S130(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S130)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S130)
       deallocate(S130)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',t2A,D2)
       allocate(S131(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S131)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S131)
       deallocate(S131)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(S132(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S132)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S132)
       deallocate(S132)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'12',FAHP,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(S133(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S133)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S133)
       deallocate(S133)
C
       allocate(B1(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,N1-N1/),'21',FAHP,B1)
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S134(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,D2,S134)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S134)
       deallocate(S134)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(S135(N1+1:M2,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K9
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S135)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2314',0.500,X6,
     & S135)
       deallocate(S135)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S136(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S136)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2314',0.500,X5,
     & S136)
       deallocate(S136)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'3412',VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S137(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S137)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2314',0.500,X4,
     & S137)
       deallocate(S137)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'4132',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S138(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S138)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',-1.000,
     & X3,S138)
       deallocate(S138)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'3142',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S139(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S139)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2314',1.000,X2,
     & S139)
       deallocate(S139)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M1-N0/),'3142',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S140(M2+1:N3,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S140)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S140)
       deallocate(S140)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'3412',VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(S141(N1+1:M2,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K9
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S141)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'2314',0.500,
     & X12,S141)
       deallocate(S141)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'3412',VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S142(M2+1:N3,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S142)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'2314',0.500,
     & X11,S142)
       deallocate(S142)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'3412',VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S143(M2+1:N3,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S143)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2314',0.500,
     & X10,S143)
       deallocate(S143)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4132',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S144(N1+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S144)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X67),size(X67),'2314',-1.000,
     & X67,S144)
       deallocate(S144)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'3142',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S145(N1+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S145)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X66),size(X66),'2314',1.000,
     & X66,S145)
       deallocate(S145)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'3142',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S146(M2+1:N3,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S146)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2413',-1.000,
     & X1,S146)
       deallocate(S146)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'3142',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S147(M2+1:N3,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S147)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X65),size(X65),'2314',1.000,
     & X65,S147)
       deallocate(S147)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'3142',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S148(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S148)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2413',-1.000,
     & X2,S148)
       deallocate(S148)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4132',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S149(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S149)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2413',1.000,X3,
     & S149)
       deallocate(S149)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M2-N1/),'4213',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S150(N1+1:M2,N0+1:M1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S150)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'3412',1.000,
     & X11,S150)
       deallocate(S150)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S151(N1+1:M2,N0+1:M1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S151)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'3412',-1.000,
     & X10,S151)
       deallocate(S151)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,M2-N1/),'1243',VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',t2A,D2)
       allocate(S152(N0+1:M1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S152)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'3412',0.500,X1,
     & S152)
       deallocate(S152)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4213',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S153(N1+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S153)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'3412',1.000,
     & X12,S153)
       deallocate(S153)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4213',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S154(N1+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S154)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'2413',-1.000,
     & X12,S154)
       deallocate(S154)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S155(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S155)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2413',1.000,
     & X10,S155)
       deallocate(S155)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,N1-N1/),'1243',VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',t2A,D2)
       allocate(S156(N0+1:M1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S156)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'3412',0.500,X2,
     & S156)
       deallocate(S156)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S157(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S157)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'2413',1.000,
     & X11,S157)
       deallocate(S157)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,N1-N1/),'1243',VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',t2A,D2)
       allocate(S158(N0+1:M1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S158)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'3412',0.500,X3,
     & S158)
       deallocate(S158)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,M2-N1/),'1243',VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(S159(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K5*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S159)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'3412',0.500,X7,
     & S159)
       deallocate(S159)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S160(N1+1:M2,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S160)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'3412',-1.000,
     & X4,S160)
       deallocate(S160)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M2-N1/),'4213',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S161(N1+1:M2,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S161)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'3412',1.000,X5,
     & S161)
       deallocate(S161)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,N1-N1/),'1243',VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(S162(N0+1:M1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K5*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S162)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'3412',0.500,X8,
     & S162)
       deallocate(S162)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S163(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S163)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2413',1.000,X4,
     & S163)
       deallocate(S163)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,N1-N1/),'1243',VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(S164(N0+1:M1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K5*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S164)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'3412',0.500,X9,
     & S164)
       deallocate(S164)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S165(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S165)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2413',1.000,X5,
     & S165)
       deallocate(S165)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4213',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S166(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S166)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'3412',1.000,X6,
     & S166)
       deallocate(S166)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4213',VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S167(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S167)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2413',-1.000,
     & X6,S167)
       deallocate(S167)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S168(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S168)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',1.000,X3,
     & S168)
       deallocate(S168)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S169(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S169)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2314',1.000,X2,
     & S169)
       deallocate(S169)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M1-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S170(M2+1:N3,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S170)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S170)
       deallocate(S170)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S171(N1+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S171)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X67),size(X67),'2314',1.000,
     & X67,S171)
       deallocate(S171)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S172(N1+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S172)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X66),size(X66),'2314',1.000,
     & X66,S172)
       deallocate(S172)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S173(M2+1:N3,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S173)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X65),size(X65),'2314',1.000,
     & X65,S173)
       deallocate(S173)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S174(N1+1:M2,N0+1:M1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S174)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'3412',1.000,
     & X11,S174)
       deallocate(S174)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S175(N1+1:M2,N0+1:M1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S175)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'3412',1.000,
     & X10,S175)
       deallocate(S175)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S176(N1+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S176)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'3412',1.000,
     & X12,S176)
       deallocate(S176)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S177(N1+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S177)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'2413',-1.000,
     & X12,S177)
       deallocate(S177)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S178(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S178)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2413',-1.000,
     & X10,S178)
       deallocate(S178)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S179(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S179)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'2413',-1.000,
     & X11,S179)
       deallocate(S179)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S180(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S180)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2413',-1.000,
     & X3,S180)
       deallocate(S180)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S181(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S181)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2413',-1.000,
     & X2,S181)
       deallocate(S181)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S182(M2+1:N3,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S182)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2413',-1.000,
     & X1,S182)
       deallocate(S182)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S183(N1+1:M2,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S183)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'3412',1.000,X5,
     & S183)
       deallocate(S183)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S184(N1+1:M2,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S184)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'3412',1.000,X4,
     & S184)
       deallocate(S184)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S185(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S185)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'3412',1.000,X6,
     & S185)
       deallocate(S185)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S186(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S186)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2413',-1.000,
     & X6,S186)
       deallocate(S186)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S187(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S187)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2413',-1.000,
     & X4,S187)
       deallocate(S187)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S188(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S188)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2413',-1.000,
     & X5,S188)
       deallocate(S188)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,N1-N1,N1-N1,N0-N0/),'461235',t3A,F2)
       allocate(S189(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S189)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',-1.000,
     & X12,S189)
       deallocate(S189)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N1,N1-N1,N1-N1,N0-N0/),'561234',t3A,F2)
       allocate(S190(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S190)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',0.500,
     & X12,S190)
       deallocate(S190)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,N1-N1,N1-N1,N0-N0/),'461235',t3A,F2)
       allocate(S191(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S191)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',-1.000,
     & X12,S191)
       deallocate(S191)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N1-N1,N1-N1,N1-N1,N0-N0/),'561234',t3A,F2)
       allocate(S192(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S192)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',0.500,
     & X12,S192)
       deallocate(S192)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,M2-N1,N1-N1,N0-N0/),'461235',t3A,F2)
       allocate(S193(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S193)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',-1.000,
     & X11,S193)
       deallocate(S193)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N1,M2-N1,N1-N1,N0-N0/),'561234',t3A,F2)
       allocate(S194(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S194)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',0.500,
     & X11,S194)
       deallocate(S194)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,M2-N1,N1-N1,N0-N0/),'462135',t3A,F2)
       allocate(S195(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S195)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',1.000,
     & X11,S195)
       deallocate(S195)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N1-N1,M2-N1,N1-N1,N0-N0/),'562134',t3A,F2)
       allocate(S196(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S196)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',-0.500,
     & X11,S196)
       deallocate(S196)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,M2-N1,N1-N1,N0-N0/),'461235',t3A,F2)
       allocate(S197(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S197)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',1.000,
     & X10,S197)
       deallocate(S197)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N1,M2-N1,N1-N1,N0-N0/),'561234',t3A,F2)
       allocate(S198(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S198)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-0.500,
     & X10,S198)
       deallocate(S198)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,M2-N1,N1-N1,N0-N0/),'462135',t3A,F2)
       allocate(S199(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S199)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S199)
       deallocate(S199)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N1-N1,M2-N1,N1-N1,N0-N0/),'562134',t3A,F2)
       allocate(S200(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S200)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',0.500,
     & X10,S200)
       deallocate(S200)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',t2A,D2)
       allocate(S201(N0+1:M1,M1+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S201)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X22),size(X22),'3412',0.500,
     & X22,S201)
       deallocate(S201)
C
       allocate(D1(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',t2A,D2)
       allocate(S202(N0+1:M1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S202)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X23),size(X23),'3412',0.500,
     & X23,S202)
       deallocate(S202)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,M2-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(S203(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S203)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',0.500,X9,
     & S203)
       deallocate(S203)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N1-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(S204(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S204)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',1.000,X9,
     & S204)
       deallocate(S204)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N1-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(S205(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S205)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',0.500,X9,
     & S205)
       deallocate(S205)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,M2-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(S206(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S206)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-0.500,
     & X8,S206)
       deallocate(S206)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N1-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(S207(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S207)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S207)
       deallocate(S207)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N1-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(S208(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S208)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-0.500,
     & X8,S208)
       deallocate(S208)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S209(M2+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S209)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X24),size(X24),'3412',-1.000,
     & X24,S209)
       deallocate(S209)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S210(M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S210)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X25),size(X25),'3412',-1.000,
     & X25,S210)
       deallocate(S210)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N1-N1,M2-N1,N0-N0,N0-N0/),'613245',t3A,F2)
       allocate(S211(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S211)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S211)
       deallocate(S211)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N1-N1,M2-N1,N0-N0,N0-N0/),'623145',t3A,F2)
       allocate(S212(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S212)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',-0.500,
     & X7,S212)
       deallocate(S212)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S213(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S213)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X26),size(X26),'3412',-1.000,
     & X26,S213)
       deallocate(S213)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S214(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S214)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X27),size(X27),'3412',-1.000,
     & X27,S214)
       deallocate(S214)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S215(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S215)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S215),size(S215),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,M1-M1/),'3412',S215,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(Z312(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z312)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z312)
       deallocate(Z312)
       deallocate(S215)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S216(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S216)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S216),size(S216),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N1-N1,M1-M1/),'3412',S216,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,N1-N1,M2-N1,N1-N1,N0-N0,N0-N0/),'621345',t3A,F2)
       allocate(Z313(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z313)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & -1.000,V3A,Z313)
       deallocate(Z313)
       deallocate(S216)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Q28(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q28)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X13),size(X13),'21',0.500,
     & X13,Q28)
       deallocate(Q28)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S217(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S217)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',0.500,X6,
     & S217)
       deallocate(S217)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S218(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S218)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',1.000,X6,
     & S218)
       deallocate(S218)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S219(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S219)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',0.500,X6,
     & S219)
       deallocate(S219)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S220(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S220)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',0.500,X6,
     & S220)
       deallocate(S220)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S221(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S221)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',1.000,X6,
     & S221)
       deallocate(S221)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S222(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S222)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',0.500,X6,
     & S222)
       deallocate(S222)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S223(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S223)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',0.500,X5,
     & S223)
       deallocate(S223)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S224(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S224)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',1.000,X5,
     & S224)
       deallocate(S224)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S225(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S225)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',0.500,X5,
     & S225)
       deallocate(S225)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,M2-N1,N1-N1,M1-M1/),'452136',t3A,F2)
       allocate(S226(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S226)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-0.500,
     & X5,S226)
       deallocate(S226)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,M2-N1,N1-N1,M1-M1/),'452136',t3A,F2)
       allocate(S227(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S227)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S227)
       deallocate(S227)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,M2-N1,N1-N1,M1-M1/),'452136',t3A,F2)
       allocate(S228(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S228)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-0.500,
     & X5,S228)
       deallocate(S228)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S229(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S229)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-0.500,
     & X4,S229)
       deallocate(S229)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S230(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S230)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S230)
       deallocate(S230)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,M2-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(S231(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S231)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-0.500,
     & X4,S231)
       deallocate(S231)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N1-N1,M2-N1,N1-N1,M1-M1/),'452136',t3A,F2)
       allocate(S232(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S232)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',0.500,X4,
     & S232)
       deallocate(S232)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N1-N1,M2-N1,N1-N1,M1-M1/),'452136',t3A,F2)
       allocate(S233(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S233)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',1.000,X4,
     & S233)
       deallocate(S233)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N1-N1,M2-N1,N1-N1,M1-M1/),'452136',t3A,F2)
       allocate(S234(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S234)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',0.500,X4,
     & S234)
       deallocate(S234)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(S235(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K5*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S235)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X30(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       X30=0.0d0
       call sum_stripe(4,shape(X30),size(X30),'3412',0.500,
     & X30,S235)
       deallocate(S235)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(S236(N0+1:M1,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K5*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S236)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X31(N0+1:M1,M1+1:N1,N0+1:M1,N0+1:M1))
       X31=0.0d0
       call sum_stripe(4,shape(X31),size(X31),'3412',0.500,
     & X31,S236)
       deallocate(S236)
C
       allocate(D1(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,M1-N0,M1-N0/),'1234',VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(S237(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K5*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S237)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X32(M1+1:N1,M1+1:N1,N0+1:M1,N0+1:M1))
       X32=0.0d0
       call sum_stripe(4,shape(X32),size(X32),'3412',0.500,
     & X32,S237)
       deallocate(S237)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,M2-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3A,F2)
       allocate(S238(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S238)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',0.500,X3,
     & S238)
       deallocate(S238)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,M2-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'512346',t3A,F2)
       allocate(S239(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S239)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-0.500,
     & X3,S239)
       deallocate(S239)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3A,F2)
       allocate(S240(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S240)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',1.000,X3,
     & S240)
       deallocate(S240)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N1-N1,N1-N1,N0-N0,M1-M1/),'512346',t3A,F2)
       allocate(S241(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S241)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S241)
       deallocate(S241)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3A,F2)
       allocate(S242(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S242)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',0.500,X3,
     & S242)
       deallocate(S242)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N1-N1,N1-N1,N0-N0,M1-M1/),'512346',t3A,F2)
       allocate(S243(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S243)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-0.500,
     & X3,S243)
       deallocate(S243)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,M2-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3A,F2)
       allocate(S244(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S244)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-0.500,
     & X2,S244)
       deallocate(S244)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,M2-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'512346',t3A,F2)
       allocate(S245(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S245)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',0.500,X2,
     & S245)
       deallocate(S245)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3A,F2)
       allocate(S246(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S246)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S246)
       deallocate(S246)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N1-N1,N1-N1,N0-N0,M1-M1/),'512346',t3A,F2)
       allocate(S247(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S247)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',1.000,X2,
     & S247)
       deallocate(S247)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3A,F2)
       allocate(S248(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S248)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-0.500,
     & X2,S248)
       deallocate(S248)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N1-N1,N1-N1,N0-N0,M1-M1/),'512346',t3A,F2)
       allocate(S249(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S249)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',0.500,X2,
     & S249)
       deallocate(S249)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S250(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S250)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X33),size(X33),'3412',-1.000,
     & X33,S250)
       deallocate(S250)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S251(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S251)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X34),size(X34),'3412',-1.000,
     & X34,S251)
       deallocate(S251)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S252(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S252)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X35),size(X35),'3412',-1.000,
     & X35,S252)
       deallocate(S252)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S253(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S253)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X36),size(X36),'3412',-1.000,
     & X36,S253)
       deallocate(S253)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N1-N1,M2-N1,N0-N0,M1-M1/),'413256',t3A,F2)
       allocate(S254(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S254)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S254)
       deallocate(S254)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N1-N1,M2-N1,N0-N0,M1-M1/),'513246',t3A,F2)
       allocate(S255(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S255)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S255)
       deallocate(S255)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N1-N1,M2-N1,N0-N0,M1-M1/),'423156',t3A,F2)
       allocate(S256(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S256)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-0.500,
     & X1,S256)
       deallocate(S256)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N1-N1,M2-N1,N0-N0,M1-M1/),'523146',t3A,F2)
       allocate(S257(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S257)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',0.500,X1,
     & S257)
       deallocate(S257)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S258(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S258)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X37),size(X37),'3412',-1.000,
     & X37,S258)
       deallocate(S258)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S259(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S259)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X38),size(X38),'3412',-1.000,
     & X38,S259)
       deallocate(S259)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S260(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S260)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X39),size(X39),'3412',-1.000,
     & X39,S260)
       deallocate(S260)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S261(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S261)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X40),size(X40),'3412',-1.000,
     & X40,S261)
       deallocate(S261)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S262(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S262)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S262),size(S262),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N0-N0/),'3412',S262,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3A,F2)
       allocate(Z360(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z360)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z360)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z360)
       deallocate(Z360)
       deallocate(S262)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S263(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S263)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S263),size(S263),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,N0-N0/),'3412',S263,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'512346',t3A,F2)
       allocate(Z361(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z361)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z361)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z361)
       deallocate(Z361)
       deallocate(S263)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S264(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S264)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S264),size(S264),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3412',S264,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'421356',t3A,F2)
       allocate(Z362(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z362)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z362)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z362)
       deallocate(Z362)
       deallocate(S264)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S265(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S265)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S265),size(S265),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N1-N1,N0-N0/),'3412',S265,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'521346',t3A,F2)
       allocate(Z363(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z363)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z363)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z363)
       deallocate(Z363)
       deallocate(S265)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(Q29(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q29)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X14),size(X14),'21',0.500,
     & X14,Q29)
       deallocate(Q29)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(Q30(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q30)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X15),size(X15),'21',0.500,
     & X15,Q30)
       deallocate(Q30)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,M2-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S266(M2+1:N3,N1+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S266)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X45),size(X45),'3412',0.500,
     & X45,S266)
       deallocate(S266)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S267(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S267)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X46),size(X46),'3412',0.500,
     & X46,S267)
       deallocate(S267)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S268(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S268)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X47),size(X47),'3412',0.500,
     & X47,S268)
       deallocate(S268)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,M2-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S269(M2+1:N3,N1+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S269)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X48),size(X48),'3412',0.500,
     & X48,S269)
       deallocate(S269)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S270(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S270)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X49),size(X49),'3412',0.500,
     & X49,S270)
       deallocate(S270)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S271(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S271)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X50),size(X50),'3412',0.500,
     & X50,S271)
       deallocate(S271)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,M2-N1/),'3412',t2A,D2)
       allocate(Q31(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q31)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X16),size(X16),'21',-0.500,
     & X16,Q31)
       deallocate(Q31)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,M2-N1/),'3412',t2A,D2)
       allocate(Q32(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q32)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X17),size(X17),'21',-0.500,
     & X17,Q32)
       deallocate(Q32)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(S272(N1+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K9
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S272)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X51),size(X51),'3412',0.500,
     & X51,S272)
       deallocate(S272)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(S273(N1+1:M2,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9
       I2=K9*K9
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S273)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X52),size(X52),'3412',0.500,
     & X52,S273)
       deallocate(S273)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(Q33(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q33)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X18),size(X18),'21',-0.500,
     & X18,Q33)
       deallocate(Q33)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(Q34(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q34)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X19),size(X19),'21',-0.500,
     & X19,Q34)
       deallocate(Q34)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M2-N1/),'3421',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(Q35(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q35)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(M2+1:N3,N1+1:M2))
       call reorder_shift(2,shape(Q35),size(Q35),shape(B1),size(B1),
     & (/M2-M2,N1-N1/),'21',Q35,B1)
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M2-N1,M2-N1,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z378(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K5*K9*K6
       I3=K6
       call EGEMM(I1,I2,I3,B1,F2,Z378)
       deallocate(B1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124563',
     & 0.500,V3A,Z378)
       deallocate(Z378)
       deallocate(Q35)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(Q36(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q36)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N1+1:M2,N1+1:M2))
       call reorder_shift(2,shape(Q36),size(Q36),shape(B1),size(B1),
     & (/N1-N1,N1-N1/),'21',Q36,B1)
       allocate(F2(N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N1-N1,M2-N1,N1-N1,N0-N0,N0-N0,M1-M1/),'213456',t3A,F2)
       allocate(Z379(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K5*K9*K6
       I3=K9
       call EGEMM(I1,I2,I3,B1,F2,Z379)
       deallocate(B1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124563',
     & -0.500,V3A,Z379)
       deallocate(Z379)
       deallocate(Q36)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-N2,N1-N1,N1-N1,N0-N0/),'451236',t3B4,F2)
       allocate(S274(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S274)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',-1.000,
     & X12,S274)
       deallocate(S274)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,N1-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(S275(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S275)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',1.000,
     & X12,S275)
       deallocate(S275)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,N1-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(S276(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S276)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',1.000,
     & X12,S276)
       deallocate(S276)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N2-N2,N1-N1,N1-N1,N0-N0/),'451236',t3B4,F2)
       allocate(S277(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S277)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',-1.000,
     & X12,S277)
       deallocate(S277)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,N1-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(S278(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S278)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',1.000,
     & X12,S278)
       deallocate(S278)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,N1-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(S279(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S279)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',1.000,
     & X12,S279)
       deallocate(S279)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-N2,M2-N1,N1-N1,N0-N0/),'451236',t3B4,F2)
       allocate(S280(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S280)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',-1.000,
     & X11,S280)
       deallocate(S280)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(S281(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S281)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',1.000,
     & X11,S281)
       deallocate(S281)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(S282(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S282)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',1.000,
     & X11,S282)
       deallocate(S282)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N2-N2,M2-N1,N1-N1,N0-N0/),'451236',t3B4,F2)
       allocate(S283(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S283)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',-1.000,
     & X11,S283)
       deallocate(S283)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(S284(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S284)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',1.000,
     & X11,S284)
       deallocate(S284)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(S285(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S285)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X11),size(X11),'2341',1.000,
     & X11,S285)
       deallocate(S285)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-N2,M2-N1,N1-N1,N0-N0/),'451236',t3B4,F2)
       allocate(S286(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S286)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S286)
       deallocate(S286)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(S287(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S287)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',1.000,
     & X10,S287)
       deallocate(S287)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(S288(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S288)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',1.000,
     & X10,S288)
       deallocate(S288)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N2-N2,M2-N1,N1-N1,N0-N0/),'451236',t3B4,F2)
       allocate(S289(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S289)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',-1.000,
     & X10,S289)
       deallocate(S289)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(S290(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S290)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',1.000,
     & X10,S290)
       deallocate(S290)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(S291(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S291)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X10),size(X10),'2341',1.000,
     & X10,S291)
       deallocate(S291)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(S292(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S292)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S292)
       deallocate(S292)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(S293(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S293)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S293)
       deallocate(S293)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(S294(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S294)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S294)
       deallocate(S294)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(S295(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S295)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S295)
       deallocate(S295)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(S296(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S296)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S296)
       deallocate(S296)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(S297(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S297)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S297)
       deallocate(S297)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(S298(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S298)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S298)
       deallocate(S298)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(S299(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S299)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S299)
       deallocate(S299)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S300(M2+1:N3,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S300)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X74(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       X74=0.0d0
       call sum_stripe(4,shape(X74),size(X74),'3412',1.000,
     & X74,S300)
       deallocate(S300)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S301(M2+1:N3,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S301)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X75(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       X75=0.0d0
       call sum_stripe(4,shape(X75),size(X75),'3412',1.000,
     & X75,S301)
       deallocate(S301)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B2),size(t3B2),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-M2,M2-M2,N0-N0,N0-N0/),'412356',t3B2,F2)
       allocate(S302(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S302)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',-1.000,
     & X7,S302)
       deallocate(S302)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,M2-N1,N0-N0,N0-N0/),'413256',t3B4,F2)
       allocate(S303(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S303)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S303)
       deallocate(S303)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,M2-N1,N0-N0,N0-N0/),'413256',t3B4,F2)
       allocate(S304(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S304)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X7),size(X7),'2341',1.000,X7,
     & S304)
       deallocate(S304)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S305(N1+1:M2,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S305)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X76(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       X76=0.0d0
       call sum_stripe(4,shape(X76),size(X76),'3412',1.000,
     & X76,S305)
       deallocate(S305)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S306(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S306)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X77(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       X77=0.0d0
       call sum_stripe(4,shape(X77),size(X77),'3412',1.000,
     & X77,S306)
       deallocate(S306)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S307(N1+1:M2,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S307)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X78(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       X78=0.0d0
       call sum_stripe(4,shape(X78),size(X78),'3412',1.000,
     & X78,S307)
       deallocate(S307)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S308(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S308)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X79(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       X79=0.0d0
       call sum_stripe(4,shape(X79),size(X79),'3412',1.000,
     & X79,S308)
       deallocate(S308)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S309(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S309)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S309)
       deallocate(S309)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S310(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S310)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S310)
       deallocate(S310)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S311(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S311)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S311)
       deallocate(S311)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S312(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S312)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S312)
       deallocate(S312)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S313(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S313)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S313)
       deallocate(S313)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S314(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S314)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S314)
       deallocate(S314)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S315(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S315)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S315)
       deallocate(S315)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,N1-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S316(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S316)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S316)
       deallocate(S316)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S317(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S317)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S317)
       deallocate(S317)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S318(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S318)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S318)
       deallocate(S318)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S319(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S319)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S319)
       deallocate(S319)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S320(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S320)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S320)
       deallocate(S320)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S321(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S321)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S321)
       deallocate(S321)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S322(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S322)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S322)
       deallocate(S322)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S323(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S323)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S323)
       deallocate(S323)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S324(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S324)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X5),size(X5),'2341',-1.000,
     & X5,S324)
       deallocate(S324)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S325(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S325)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S325)
       deallocate(S325)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S326(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S326)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S326)
       deallocate(S326)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S327(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S327)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S327)
       deallocate(S327)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S328(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S328)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S328)
       deallocate(S328)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N0-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S329(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S329)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S329)
       deallocate(S329)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N0-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S330(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S330)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S330)
       deallocate(S330)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S331(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S331)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S331)
       deallocate(S331)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-N0,N2-N2,M2-N1,N1-N1,M1-M1/),'451236',t3B1,F2)
       allocate(S332(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S332)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X4),size(X4),'2341',-1.000,
     & X4,S332)
       deallocate(S332)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S333(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S333)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S333)
       deallocate(S333)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S334(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S334)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S334)
       deallocate(S334)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S335(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S335)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S335)
       deallocate(S335)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S336(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S336)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S336)
       deallocate(S336)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S337(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S337)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S337)
       deallocate(S337)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S338(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S338)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S338)
       deallocate(S338)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S339(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S339)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S339)
       deallocate(S339)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S340(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S340)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S340)
       deallocate(S340)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S341(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S341)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S341)
       deallocate(S341)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S342(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S342)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S342)
       deallocate(S342)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S343(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S343)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S343)
       deallocate(S343)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S344(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S344)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S344)
       deallocate(S344)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S345(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S345)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S345)
       deallocate(S345)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S346(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S346)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S346)
       deallocate(S346)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S347(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S347)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S347)
       deallocate(S347)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(S348(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S348)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S348)
       deallocate(S348)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S349(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S349)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X80(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X80=0.0d0
       call sum_stripe(4,shape(X80),size(X80),'3412',1.000,
     & X80,S349)
       deallocate(S349)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S350(M2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S350)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X81(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       X81=0.0d0
       call sum_stripe(4,shape(X81),size(X81),'3412',1.000,
     & X81,S350)
       deallocate(S350)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S351(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S351)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X82(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       X82=0.0d0
       call sum_stripe(4,shape(X82),size(X82),'3412',1.000,
     & X82,S351)
       deallocate(S351)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S352(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S352)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X83(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       X83=0.0d0
       call sum_stripe(4,shape(X83),size(X83),'3412',1.000,
     & X83,S352)
       deallocate(S352)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-M2,M2-M2,N0-N0,M1-M1/),'412356',t3B3,F2)
       allocate(S353(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S353)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S353)
       deallocate(S353)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B3),size(t3B3),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-M2,M2-M2,N0-N0,M1-M1/),'412356',t3B3,F2)
       allocate(S354(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S354)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',-1.000,
     & X1,S354)
       deallocate(S354)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,M2-N1,N0-N0,M1-M1/),'413256',t3B1,F2)
       allocate(S355(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S355)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S355)
       deallocate(S355)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,M2-N1,N0-N0,M1-M1/),'413256',t3B1,F2)
       allocate(S356(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S356)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S356)
       deallocate(S356)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,M2-N1,N0-N0,M1-M1/),'413256',t3B1,F2)
       allocate(S357(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S357)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S357)
       deallocate(S357)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,M2-N1,N0-N0,M1-M1/),'413256',t3B1,F2)
       allocate(S358(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S358)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S358)
       deallocate(S358)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S359(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S359)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X84(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X84=0.0d0
       call sum_stripe(4,shape(X84),size(X84),'3412',1.000,
     & X84,S359)
       deallocate(S359)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S360(N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S360)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X85(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       X85=0.0d0
       call sum_stripe(4,shape(X85),size(X85),'3412',1.000,
     & X85,S360)
       deallocate(S360)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S361(N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S361)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X86(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       X86=0.0d0
       call sum_stripe(4,shape(X86),size(X86),'3412',1.000,
     & X86,S361)
       deallocate(S361)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S362(N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S362)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X87(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       X87=0.0d0
       call sum_stripe(4,shape(X87),size(X87),'3412',1.000,
     & X87,S362)
       deallocate(S362)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S363(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S363)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X88(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X88=0.0d0
       call sum_stripe(4,shape(X88),size(X88),'3412',1.000,
     & X88,S363)
       deallocate(S363)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S364(N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S364)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X89(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       X89=0.0d0
       call sum_stripe(4,shape(X89),size(X89),'3412',1.000,
     & X89,S364)
       deallocate(S364)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S365(N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S365)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X90(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       X90=0.0d0
       call sum_stripe(4,shape(X90),size(X90),'3412',1.000,
     & X90,S365)
       deallocate(S365)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S366(N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S366)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X91(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       X91=0.0d0
       call sum_stripe(4,shape(X91),size(X91),'3412',1.000,
     & X91,S366)
       deallocate(S366)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S367(M2+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S367)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X24),size(X24),'3412',-1.000,
     & X24,S367)
       deallocate(S367)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S368(M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S368)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X25),size(X25),'3412',-1.000,
     & X25,S368)
       deallocate(S368)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S369(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S369)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X26),size(X26),'3412',-1.000,
     & X26,S369)
       deallocate(S369)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S370(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S370)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X27),size(X27),'3412',-1.000,
     & X27,S370)
       deallocate(S370)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S371(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S371)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X28),size(X28),'3412',-1.000,
     & X28,S371)
       deallocate(S371)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,M1,N1,X28,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(Z28(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X28,F2,Z28)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & -1.000,V3A,Z28)
       deallocate(Z28)
       deallocate(X28)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S372(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S372)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X29),size(X29),'3412',-1.000,
     & X29,S372)
       deallocate(S372)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,M1,N1,X29,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,N1-N1,M2-N1,N1-N1,N0-N0,N0-N0/),'621345',t3A,F2)
       allocate(Z29(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X29,F2,Z29)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z29)
       deallocate(Z29)
       deallocate(X29)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(Q37(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q37)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X13),size(X13),'21',1.000,
     & X13,Q37)
       deallocate(Q37)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S373(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S373)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X33),size(X33),'3412',-1.000,
     & X33,S373)
       deallocate(S373)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S374(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S374)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X34),size(X34),'3412',-1.000,
     & X34,S374)
       deallocate(S374)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S375(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S375)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X35),size(X35),'3412',-1.000,
     & X35,S375)
       deallocate(S375)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S376(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S376)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X36),size(X36),'3412',-1.000,
     & X36,S376)
       deallocate(S376)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S377(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S377)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X37),size(X37),'3412',-1.000,
     & X37,S377)
       deallocate(S377)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S378(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S378)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X38),size(X38),'3412',-1.000,
     & X38,S378)
       deallocate(S378)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S379(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S379)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X39),size(X39),'3412',-1.000,
     & X39,S379)
       deallocate(S379)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S380(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S380)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X40),size(X40),'3412',-1.000,
     & X40,S380)
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
       call sum_stripe(4,shape(X41),size(X41),'3412',-1.000,
     & X41,S381)
       deallocate(S381)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,M1,X41,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3A,F2)
       allocate(Z41(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X41,F2,Z41)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z41)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z41)
       deallocate(Z41)
       deallocate(X41)
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
       call sum_stripe(4,shape(X42),size(X42),'3412',-1.000,
     & X42,S382)
       deallocate(S382)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,M1,X42,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'512346',t3A,F2)
       allocate(Z42(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X42,F2,Z42)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z42)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z42)
       deallocate(Z42)
       deallocate(X42)
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
       call sum_stripe(4,shape(X43),size(X43),'3412',-1.000,
     & X43,S383)
       deallocate(S383)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,M1,X43,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'421356',t3A,F2)
       allocate(Z43(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X43,F2,Z43)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z43)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z43)
       deallocate(Z43)
       deallocate(X43)
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
       call sum_stripe(4,shape(X44),size(X44),'3412',-1.000,
     & X44,S384)
       deallocate(S384)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,M1,X44,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'521346',t3A,F2)
       allocate(Z44(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X44,F2,Z44)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z44)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z44)
       deallocate(Z44)
       deallocate(X44)
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
       call sum_stripe(2,shape(X14),size(X14),'21',1.000,
     & X14,Q38)
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
       call sum_stripe(2,shape(X15),size(X15),'21',1.000,
     & X15,Q39)
       deallocate(Q39)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',t2B,D2)
       allocate(Q40(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q40)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X16),size(X16),'21',-1.000,
     & X16,Q40)
       deallocate(Q40)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',t2B,D2)
       allocate(Q41(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q41)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X17),size(X17),'21',-1.000,
     & X17,Q41)
       deallocate(Q41)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(Q42(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q42)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X18),size(X18),'21',-1.000,
     & X18,Q42)
       deallocate(Q42)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(Q43(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q43)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X19),size(X19),'21',-1.000,
     & X19,Q43)
       deallocate(Q43)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,M2-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(Q44(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q44)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X20),size(X20),'21',-1.000,
     & X20,Q44)
       deallocate(Q44)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(Q45(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q45)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X21),size(X21),'21',-1.000,
     & X21,Q45)
       deallocate(Q45)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S385(M2+1:N3,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S385)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X53(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       X53=0.0d0
       call sum_stripe(4,shape(X53),size(X53),'3412',1.000,
     & X53,S385)
       deallocate(S385)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,M2,N3,M1,N1,X53,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z53(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,X53,F2,Z53)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z53)
       deallocate(Z53)
       deallocate(X53)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S386(M2+1:N3,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S386)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X54(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       X54=0.0d0
       call sum_stripe(4,shape(X54),size(X54),'3412',1.000,
     & X54,S386)
       deallocate(S386)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,M2,N3,M1,N1,X54,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z54(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,X54,F2,Z54)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z54)
       deallocate(Z54)
       deallocate(X54)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S387(N1+1:M2,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S387)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X55(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       X55=0.0d0
       call sum_stripe(4,shape(X55),size(X55),'3412',1.000,
     & X55,S387)
       deallocate(S387)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,N1,M2,M1,N1,X55,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z55(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X55,F2,Z55)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z55)
       deallocate(Z55)
       deallocate(X55)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S388(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S388)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X56(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       X56=0.0d0
       call sum_stripe(4,shape(X56),size(X56),'3412',1.000,
     & X56,S388)
       deallocate(S388)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,N1,M2,M1,N1,X56,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z56(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X56,F2,Z56)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z56)
       deallocate(Z56)
       deallocate(X56)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S389(N1+1:M2,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S389)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X78),size(X78),'3412',1.000,
     & X78,S389)
       deallocate(S389)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S390(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S390)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X79),size(X79),'3412',1.000,
     & X79,S390)
       deallocate(S390)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S391(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S391)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X57(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X57=0.0d0
       call sum_stripe(4,shape(X57),size(X57),'3412',1.000,
     & X57,S391)
       deallocate(S391)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,M1,X57,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z59(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K6*K5
       call EGEMM(I1,I2,I3,X57,F2,Z59)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z59)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z59)
       deallocate(Z59)
       deallocate(X57)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S392(M2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S392)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X58(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       X58=0.0d0
       call sum_stripe(4,shape(X58),size(X58),'3412',1.000,
     & X58,S392)
       deallocate(S392)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,M1,X58,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z60(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,X58,F2,Z60)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z60)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z60)
       deallocate(Z60)
       deallocate(X58)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S393(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S393)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X59(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       X59=0.0d0
       call sum_stripe(4,shape(X59),size(X59),'3412',1.000,
     & X59,S393)
       deallocate(S393)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,M1,X59,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z61(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K0*K5
       call EGEMM(I1,I2,I3,X59,F2,Z61)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z61)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z61)
       deallocate(Z61)
       deallocate(X59)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S394(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S394)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X60(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       X60=0.0d0
       call sum_stripe(4,shape(X60),size(X60),'3412',1.000,
     & X60,S394)
       deallocate(S394)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,M1,X60,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z62(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,X60,F2,Z62)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z62)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z62)
       deallocate(Z62)
       deallocate(X60)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S395(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S395)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X61(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X61=0.0d0
       call sum_stripe(4,shape(X61),size(X61),'3412',1.000,
     & X61,S395)
       deallocate(S395)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,M1,X61,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z63(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X61,F2,Z63)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z63)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z63)
       deallocate(Z63)
       deallocate(X61)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S396(N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S396)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X62(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       X62=0.0d0
       call sum_stripe(4,shape(X62),size(X62),'3412',1.000,
     & X62,S396)
       deallocate(S396)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,M1,X62,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z64(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X62,F2,Z64)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z64)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z64)
       deallocate(Z64)
       deallocate(X62)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S397(N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S397)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X63(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       X63=0.0d0
       call sum_stripe(4,shape(X63),size(X63),'3412',1.000,
     & X63,S397)
       deallocate(S397)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,M1,X63,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z65(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X63,F2,Z65)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z65)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z65)
       deallocate(Z65)
       deallocate(X63)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S398(N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S398)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X64(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       X64=0.0d0
       call sum_stripe(4,shape(X64),size(X64),'3412',1.000,
     & X64,S398)
       deallocate(S398)
C
       call sumx_sorted1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,M1,X64,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z66(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X64,F2,Z66)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z66)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z66)
       deallocate(Z66)
       deallocate(X64)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S399(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S399)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X88),size(X88),'3412',1.000,
     & X88,S399)
       deallocate(S399)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S400(N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S400)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X89),size(X89),'3412',1.000,
     & X89,S400)
       deallocate(S400)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S401(N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S401)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X90),size(X90),'3412',1.000,
     & X90,S401)
       deallocate(S401)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S402(N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S402)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X91),size(X91),'3412',1.000,
     & X91,S402)
       deallocate(S402)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M1-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S403(N0+1:M1,N0+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S403)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S403),size(S403),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S403,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S405(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S405)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S405)
       deallocate(S405)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S403),size(S403),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3214',S403,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S406(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S406)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',1.000,X3,
     & S406)
       deallocate(S406)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S403),size(S403),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S403,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S404(M2+1:N3,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S404)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-1.000,
     & X1,S404)
       deallocate(S404)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S407(N1+1:M2,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S407)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S407),size(S407),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2314',S407,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S411(N1+1:M2,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S411)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'3124',-1.000,
     & X6,S411)
       deallocate(S411)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S407),size(S407),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2314',S407,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S408(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S408)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',1.000,X4,
     & S408)
       deallocate(S408)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,M1-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S409(N1+1:M2,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S409)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S409),size(S409),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2314',S409,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S410(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S410)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',1.000,X5,
     & S410)
       deallocate(S410)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N0-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S412(M1+1:N1,N0+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S412)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S412),size(S412),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2341',S412,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S414(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S414)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',1.000,X2,
     & S414)
       deallocate(S414)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S412),size(S412),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3241',S412,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S415(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S415)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',-1.000,
     & X3,S415)
       deallocate(S415)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S412),size(S412),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2341',S412,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S413(M2+1:N3,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S413)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',1.000,X1,
     & S413)
       deallocate(S413)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,M2-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S416(M1+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S416)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S416),size(S416),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'2341',S416,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S422(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S422)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'3124',-1.000,
     & X4,S422)
       deallocate(S422)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S416),size(S416),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,M1-M1/),'2341',S416,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S423(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S423)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'3124',-1.000,
     & X5,S423)
       deallocate(S423)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S416),size(S416),shape(D1),size(D1),
     & (/N1-N1,N0-N0,M2-M2,M1-M1/),'3241',S416,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S417(N0+1:M1,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S417)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'3124',1.000,X1,
     & S417)
       deallocate(S417)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N1-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S418(M1+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S418)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S418),size(S418),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2341',S418,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S424(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S424)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',1.000,X4,
     & S424)
       deallocate(S424)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S418),size(S418),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2341',S418,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S426(N1+1:M2,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S426)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'3124',-1.000,
     & X6,S426)
       deallocate(S426)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S418),size(S418),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N1-N1,M1-M1/),'3241',S418,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S419(N0+1:M1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S419)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'3124',1.000,X2,
     & S419)
       deallocate(S419)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N1-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S420(M1+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S420)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S420),size(S420),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N1-N1,M1-M1/),'3241',S420,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S421(N0+1:M1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S421)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'3124',1.000,X3,
     & S421)
       deallocate(S421)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S420),size(S420),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2341',S420,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S427(N1+1:M2,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S427)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'2134',1.000,X6,
     & S427)
       deallocate(S427)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S420),size(S420),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2341',S420,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S425(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S425)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',1.000,X5,
     & S425)
       deallocate(S425)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N0-N0/),'1342',VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S428(N0+1:M1,N0+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S428)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S428),size(S428),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S428,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S430(N1+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S430)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X66),size(X66),'2134',-1.000,
     & X66,S430)
       deallocate(S430)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S428),size(S428),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3214',S428,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S431(N1+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S431)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X67),size(X67),'2134',1.000,
     & X67,S431)
       deallocate(S431)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S428),size(S428),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S428,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S429(M2+1:N3,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S429)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X65),size(X65),'2134',-1.000,
     & X65,S429)
       deallocate(S429)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S432(N1+1:M2,N0+1:N1,N1+1:N3,N0+1:M1))
       I1=K5*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S432)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S432),size(S432),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2314',S432,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S436(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S436)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'3124',-1.000,
     & X12,S436)
       deallocate(S436)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S432),size(S432),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2314',S432,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S433(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S433)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',1.000,
     & X10,S433)
       deallocate(S433)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N0-N0/),'4312',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S434(N1+1:M2,N0+1:N1,N1+1:N3,N0+1:M1))
       I1=K5*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S434)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S434),size(S434),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2314',S434,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S435(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S435)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'2134',1.000,
     & X11,S435)
       deallocate(S435)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,M2-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S437(N0+1:M1,N0+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S437)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S437),size(S437),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,N0-N0/),'2341',S437,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S443(N1+1:M2,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S443)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'3124',-1.000,
     & X10,S443)
       deallocate(S443)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S437),size(S437),shape(D1),size(D1),
     & (/N0-N0,N1-N1,M2-M2,N0-N0/),'2341',S437,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S444(N1+1:M2,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S444)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'3124',-1.000,
     & X11,S444)
       deallocate(S444)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S437),size(S437),shape(D1),size(D1),
     & (/N1-N1,N0-N0,M2-M2,N0-N0/),'3241',S437,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S438(N0+1:M1,N0+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S438)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'3124',1.000,X7,
     & S438)
       deallocate(S438)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N1-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S439(N0+1:M1,N0+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S439)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S439),size(S439),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2341',S439,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S445(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S445)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',1.000,
     & X10,S445)
       deallocate(S445)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S439),size(S439),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2341',S439,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S447(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S447)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'3124',-1.000,
     & X12,S447)
       deallocate(S447)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S439),size(S439),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N1-N1,N0-N0/),'3241',S439,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S440(N0+1:M1,N0+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S440)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'3124',1.000,X8,
     & S440)
       deallocate(S440)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N1-N1/),'2413',VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S441(N0+1:M1,N0+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S441)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S441),size(S441),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N1-N1,N0-N0/),'3241',S441,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S442(N0+1:M1,N0+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S442)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'3124',1.000,X9,
     & S442)
       deallocate(S442)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S441),size(S441),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2341',S441,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S448(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S448)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'2134',1.000,
     & X12,S448)
       deallocate(S448)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S441),size(S441),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2341',S441,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S446(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S446)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'2134',1.000,
     & X11,S446)
       deallocate(S446)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S449(M1+1:N1,N0+1:M1,M1+1:N1,N1+1:N3))
       I1=K3*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S449)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S449),size(S449),shape(D1),size(D1),
     & (/N1-N1,N0-N0,M1-M1,M1-M1/),'4231',S449,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S450(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S450)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X22),size(X22),'3124',1.000,
     & X22,S450)
       deallocate(S450)
C
       call sumx_sorted3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,N0,M1,M1,N1,X22,VAHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,N1-N1,N1-N1,N0-N0/),'461235',t3A,F2)
       allocate(Z22(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K5*K9*K9*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,X22,F2,Z22)
       deallocate(F2)
C
       V3A=V3A-Z22
       call sum_stripe(6,shape(V3A),size(V3A),'123546',
     & 1.000,V3A,Z22)
       deallocate(Z22)
       deallocate(X22)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S451(M1+1:N1,M1+1:N1,M1+1:N1,N1+1:N3))
       I1=K3*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S451)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S451),size(S451),shape(D1),size(D1),
     & (/N1-N1,M1-M1,M1-M1,M1-M1/),'4231',S451,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S452(N0+1:M1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S452)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X23),size(X23),'3124',1.000,
     & X23,S452)
       deallocate(S452)
C
       call sumx_sorted3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,N0,M1,M1,N1,X23,VAHHHH, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N1,N1-N1,N1-N1,N0-N0/),'561234',t3A,F2)
       allocate(Z23(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K5*K9*K9*K6
       I3=K7*K7
       call EGEMM(I1,I2,I3,X23,F2,Z23)
       deallocate(F2)
C
       V3A=V3A+0.500*Z23
       call sum_stripe(6,shape(V3A),size(V3A),'123546',
     & -0.500,V3A,Z23)
       deallocate(Z23)
       deallocate(X23)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,M2-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S453(M1+1:N1,N0+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S453)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S453),size(S453),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S453,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S457(N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S457)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X26),size(X26),'3124',1.000,
     & X26,S457)
       deallocate(S457)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,M1,N1,X26,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(Z26(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X26,F2,Z26)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & 1.000,V3A,Z26)
       deallocate(Z26)
       deallocate(X26)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S453),size(S453),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'2341',S453,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S454(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S454)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X24),size(X24),'3124',1.000,
     & X24,S454)
       deallocate(S454)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M1,N1,X24,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N1-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(Z24(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K9
       I3=K6*K7
       call EGEMM(I1,I2,I3,X24,F2,Z24)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & -1.000,V3A,Z24)
       deallocate(Z24)
       deallocate(X24)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S455(M1+1:N1,N0+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S455)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S455),size(S455),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'2341',S455,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S458(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S458)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X27),size(X27),'3124',1.000,
     & X27,S458)
       deallocate(S458)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,M1,N1,X27,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,N1-N1,M2-N1,N1-N1,N0-N0,N0-N0/),'621345',t3A,F2)
       allocate(Z27(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X27,F2,Z27)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z27)
       deallocate(Z27)
       deallocate(X27)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S455),size(S455),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'2341',S455,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S456(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S456)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X25),size(X25),'3124',1.000,
     & X25,S456)
       deallocate(S456)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,M1,N1,X25,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N1-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(Z25(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K9
       I3=K9*K7
       call EGEMM(I1,I2,I3,X25,F2,Z25)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & -1.000,V3A,Z25)
       deallocate(Z25)
       deallocate(X25)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,M2-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S459(M1+1:N1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S459)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S459),size(S459),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'3241',S459,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S460(N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S460)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X68),size(X68),'3124',-1.000,
     & X68,S460)
       deallocate(S460)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(Z119(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X68,F2,Z119)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & -1.000,V3A,Z119)
       deallocate(Z119)
       deallocate(X68)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S461(M1+1:N1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S461)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S461),size(S461),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,M1-M1/),'3241',S461,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S462(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S462)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X69),size(X69),'3124',-1.000,
     & X69,S462)
       deallocate(S462)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,N1-N1,M2-N1,N1-N1,N0-N0,N0-N0/),'621345',t3A,F2)
       allocate(Z120(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X69,F2,Z120)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z120)
       deallocate(Z120)
       deallocate(X69)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q46(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(Q46),size(Q46),shape(B1),size(B1),
     & (/N1-N1,M1-M1/),'21',Q46,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(Q50(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q50)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X15),size(X15),'21',1.000,
     & X15,Q50)
       deallocate(Q50)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(Q46),size(Q46),shape(B1),size(B1),
     & (/N1-N1,M1-M1/),'21',Q46,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(Q47(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q47)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X13),size(X13),'21',1.000,
     & X13,Q47)
       deallocate(Q47)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S465(N0+1:M1,N0+1:M1,M1+1:N1,N1+1:N3))
       I1=K3*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S465)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S465),size(S465),shape(D1),size(D1),
     & (/N1-N1,N0-N0,M1-M1,N0-N0/),'4231',S465,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S466(N0+1:M1,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S466)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X31),size(X31),'3124',1.000,
     & X31,S466)
       deallocate(S466)
C
       call sumx_sorted3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,N0,M1,N0,M1,X31,VAHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-N0,M2-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z31(M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K9*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,X31,F2,Z31)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'123645',
     & 1.000,V3A,Z31)
       deallocate(Z31)
       deallocate(X31)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S467(N0+1:M1,M1+1:N1,M1+1:N1,N1+1:N3))
       I1=K3*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S467)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S467),size(S467),shape(D1),size(D1),
     & (/N1-N1,M1-M1,M1-M1,N0-N0/),'4231',S467,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S468(N0+1:M1,M1+1:N1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S468)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X32),size(X32),'3124',1.000,
     & X32,S468)
       deallocate(S468)
C
       call sumx_sorted3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,N0,M1,N0,M1,X32,VAHHHH, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-N0,M2-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z32(M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K9*K6
       I3=K7*K7
       call EGEMM(I1,I2,I3,X32,F2,Z32)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'123645',
     & 0.500,V3A,Z32)
       deallocate(Z32)
       deallocate(X32)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M2-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S469(N0+1:M1,N0+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S469)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S469),size(S469),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S469,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S477(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S477)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X37),size(X37),'3124',1.000,
     & X37,S477)
       deallocate(S477)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,M1,X37,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3A,F2)
       allocate(Z37(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X37,F2,Z37)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & -1.000,V3A,Z37)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & 1.000,V3A,Z37)
       deallocate(Z37)
       deallocate(X37)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S469),size(S469),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'2341',S469,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S470(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S470)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X33),size(X33),'3124',1.000,
     & X33,S470)
       deallocate(S470)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,M1,X33,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3A,F2)
       allocate(Z33(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K6*K5
       call EGEMM(I1,I2,I3,X33,F2,Z33)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & 1.000,V3A,Z33)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & -1.000,V3A,Z33)
       deallocate(Z33)
       deallocate(X33)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:M1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S473(N0+1:M1,N0+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S473)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S473),size(S473),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S473,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S479(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S479)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X39),size(X39),'3124',1.000,
     & X39,S479)
       deallocate(S479)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,M1,X39,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'421356',t3A,F2)
       allocate(Z39(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X39,F2,Z39)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z39)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z39)
       deallocate(Z39)
       deallocate(X39)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S473),size(S473),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S473,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S474(M2+1:N3,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S474)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X35),size(X35),'3124',1.000,
     & X35,S474)
       deallocate(S474)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,M2,N3,N0,M1,X35,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3A,F2)
       allocate(Z35(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K9*K5
       call EGEMM(I1,I2,I3,X35,F2,Z35)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & 1.000,V3A,Z35)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & -1.000,V3A,Z35)
       deallocate(Z35)
       deallocate(X35)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,M2-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S471(N0+1:M1,N0+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S471)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S471),size(S471),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S471,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S478(N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S478)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X38),size(X38),'3124',1.000,
     & X38,S478)
       deallocate(S478)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,M1,X38,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'512346',t3A,F2)
       allocate(Z38(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X38,F2,Z38)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z38)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z38)
       deallocate(Z38)
       deallocate(X38)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S471),size(S471),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S471,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S472(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S472)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X34),size(X34),'3124',1.000,
     & X34,S472)
       deallocate(S472)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,M1,X34,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N1-N1,N1-N1,N0-N0,M1-M1/),'512346',t3A,F2)
       allocate(Z34(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K6*K7
       call EGEMM(I1,I2,I3,X34,F2,Z34)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z34)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z34)
       deallocate(Z34)
       deallocate(X34)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S475(N0+1:M1,N0+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S475)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S475),size(S475),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,N0-N0/),'2341',S475,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S480(N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S480)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X40),size(X40),'3124',1.000,
     & X40,S480)
       deallocate(S480)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,M1,X40,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'521346',t3A,F2)
       allocate(Z40(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X40,F2,Z40)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & -1.000,V3A,Z40)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & 1.000,V3A,Z40)
       deallocate(Z40)
       deallocate(X40)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S475),size(S475),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,N0-N0/),'2341',S475,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S476(M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S476)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X36),size(X36),'3124',1.000,
     & X36,S476)
       deallocate(S476)
C
       call sumx_sorted2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,N0,M1,X36,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,N1-N1,N1-N1,N0-N0,M1-M1/),'512346',t3A,F2)
       allocate(Z36(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K9*K7
       call EGEMM(I1,I2,I3,X36,F2,Z36)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z36)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z36)
       deallocate(Z36)
       deallocate(X36)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M2-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S481(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S481)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S481),size(S481),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'3241',S481,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S482(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S482)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X70),size(X70),'3124',-1.000,
     & X70,S482)
       deallocate(S482)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3A,F2)
       allocate(Z141(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X70,F2,Z141)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z141)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z141)
       deallocate(Z141)
       deallocate(X70)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,M2-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S483(N0+1:M1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S483)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S483),size(S483),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'3241',S483,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S484(N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S484)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X71),size(X71),'3124',-1.000,
     & X71,S484)
       deallocate(S484)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'512346',t3A,F2)
       allocate(Z142(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X71,F2,Z142)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z142)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z142)
       deallocate(Z142)
       deallocate(X71)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S485(N0+1:M1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S485)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S485),size(S485),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'3241',S485,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S486(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S486)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X72),size(X72),'3124',-1.000,
     & X72,S486)
       deallocate(S486)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N1-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'421356',t3A,F2)
       allocate(Z143(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X72,F2,Z143)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z143)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z143)
       deallocate(Z143)
       deallocate(X72)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S487(N0+1:M1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S487)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S487),size(S487),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N1-N1,N0-N0/),'3241',S487,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S488(N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S488)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X73),size(X73),'3124',-1.000,
     & X73,S488)
       deallocate(S488)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,N1-N1,M2-N1,N1-N1,N0-N0,M1-M1/),'521346',t3A,F2)
       allocate(Z144(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X73,F2,Z144)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & 1.000,V3A,Z144)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & -1.000,V3A,Z144)
       deallocate(Z144)
       deallocate(X73)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q48(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q48),size(Q48),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q48,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(Q49(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q49)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X14),size(X14),'21',1.000,
     & X14,Q49)
       deallocate(Q49)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S463(N0+1:M1,N0+1:M1,N0+1:M1,N1+1:N3))
       I1=K3*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S463)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S463),size(S463),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N0-N0,N0-N0/),'4231',S463,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S464(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S464)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X30),size(X30),'3124',1.000,
     & X30,S464)
       deallocate(S464)
C
       call sumx_sorted3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,M1,N0,M1,N0,M1,N0,M1,X30,VAHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1,N1-N1,M1-M1/),'451236',t3A,F2)
       allocate(Z30(M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K9*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,X30,F2,Z30)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'123645',
     & 0.500,V3A,Z30)
       deallocate(Z30)
       deallocate(X30)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,M2-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S489(N1+1:M2,N0+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S489)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S489),size(S489),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N1-N1/),'2341',S489,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S490(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S490)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X45),size(X45),'3124',1.000,
     & X45,S490)
       deallocate(S490)
C
       call sumx_sorted1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & M2,N3,M2,N3,M2,N3,N1,M2,X45,VAAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M2-N1,M2-N1,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z45(N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K5*K5*K9
       I3=K6*K6
       call EGEMM(I1,I2,I3,X45,F2,Z45)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'345612',
     & 0.500,V3A,Z45)
       deallocate(Z45)
       deallocate(X45)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S491(N1+1:M2,N0+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S491)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S491),size(S491),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N1-N1/),'2341',S491,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S501(N1+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S501)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X51),size(X51),'4123',-1.000,
     & X51,S501)
       deallocate(S501)
C
       call sumx_sorted1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & M2,N3,N1,M2,N1,M2,N1,M2,X51,VAAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M2-N1,N1-N1,M2-N1,N0-N0,N0-N0,M1-M1/),'132456',t3A,F2)
       allocate(Z51(M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9
       I2=K7*K5*K5*K6
       I3=K9*K6
       call EGEMM(I1,I2,I3,X51,F2,Z51)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'145623',
     & -1.000,V3A,Z51)
       deallocate(Z51)
       deallocate(X51)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S491),size(S491),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N1-N1/),'2341',S491,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S492(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S492)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X46),size(X46),'3124',1.000,
     & X46,S492)
       deallocate(S492)
C
       call sumx_sorted1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & M2,N3,N1,M2,M2,N3,N1,M2,X46,VAAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M2-N1,N1-N1,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z46(N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K5*K5*K9
       I3=K9*K6
       call EGEMM(I1,I2,I3,X46,F2,Z46)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'345612',
     & 1.000,V3A,Z46)
       deallocate(Z46)
       deallocate(X46)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,M2-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S495(N1+1:M2,N0+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S495)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S495),size(S495),shape(D1),size(D1),
     & (/N0-N0,M2-M2,M2-M2,N1-N1/),'2341',S495,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S496(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S496)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X48),size(X48),'3124',1.000,
     & X48,S496)
       deallocate(S496)
C
       call sumx_sorted1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & M2,N3,M2,N3,M2,N3,N1,M2,X48,VAAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M2-N1,M2-N1,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z48(N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K5*K5*K9
       I3=K6*K6
       call EGEMM(I1,I2,I3,X48,F2,Z48)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'245613',
     & -0.500,V3A,Z48)
       deallocate(Z48)
       deallocate(X48)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,M2-N1,N1-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S497(N1+1:M2,N0+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S497)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S497),size(S497),shape(D1),size(D1),
     & (/N0-N0,M2-M2,N1-N1,N1-N1/),'2341',S497,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S498(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S498)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X49),size(X49),'3124',1.000,
     & X49,S498)
       deallocate(S498)
C
       call sumx_sorted1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & M2,N3,N1,M2,M2,N3,N1,M2,X49,VAAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M2-N1,N1-N1,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z49(N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K5*K5*K9
       I3=K9*K6
       call EGEMM(I1,I2,I3,X49,F2,Z49)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'245613',
     & -1.000,V3A,Z49)
       deallocate(Z49)
       deallocate(X49)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S499(N1+1:M2,N0+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S499)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S499),size(S499),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N1-N1/),'2341',S499,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S500(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S500)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X50),size(X50),'3124',1.000,
     & X50,S500)
       deallocate(S500)
C
       call sumx_sorted1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,M2,N1,M2,M2,N3,N1,M2,X50,VAAPPP, 1.000)
C
       allocate(F2(N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N1-N1,N1-N1,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z50(N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K5*K5*K9
       I3=K9*K9
       call EGEMM(I1,I2,I3,X50,F2,Z50)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'245613',
     & -0.500,V3A,Z50)
       deallocate(Z50)
       deallocate(X50)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q51(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q55(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q51,B2,Q55)
       deallocate(B2)
C
       call sum_stripe(2,shape(X18),size(X18),'21',-1.000,
     & X18,Q55)
       deallocate(Q55)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q57(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q51,B2,Q57)
       deallocate(B2)
C
       call sum_stripe(2,shape(X20),size(X20),'21',-1.000,
     & X20,Q57)
       deallocate(Q57)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q52(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q51,B2,Q52)
       deallocate(B2)
C
       call sum_stripe(2,shape(X16),size(X16),'21',-1.000,
     & X16,Q52)
       deallocate(Q52)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S493(N1+1:M2,N0+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S493)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S493),size(S493),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N1-N1/),'2341',S493,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S494(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S494)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X47),size(X47),'3124',1.000,
     & X47,S494)
       deallocate(S494)
C
       call sumx_sorted1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,M2,N1,M2,M2,N3,N1,M2,X47,VAAPPP, 1.000)
C
       allocate(F2(N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N1-N1,N1-N1,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z47(N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K5*K5*K9
       I3=K9*K9
       call EGEMM(I1,I2,I3,X47,F2,Z47)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'345612',
     & 0.500,V3A,Z47)
       deallocate(Z47)
       deallocate(X47)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S493),size(S493),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N1-N1/),'2341',S493,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S502(N1+1:M2,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S502)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X52),size(X52),'4123',-1.000,
     & X52,S502)
       deallocate(S502)
C
       call sumx_sorted1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,M2,N1,M2,N1,M2,N1,M2,X52,VAAPPP, 1.000)
C
       allocate(F2(N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N1-N1,N1-N1,M2-N1,N0-N0,N0-N0,M1-M1/),'231456',t3A,F2)
       allocate(Z52(M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9
       I2=K7*K5*K5*K6
       I3=K9*K9
       call EGEMM(I1,I2,I3,X52,F2,Z52)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'145623',
     & 0.500,V3A,Z52)
       deallocate(Z52)
       deallocate(X52)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q53(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q54(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q53,B2,Q54)
       deallocate(B2)
C
       call sum_stripe(2,shape(X17),size(X17),'21',-1.000,
     & X17,Q54)
       deallocate(Q54)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q58(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q53,B2,Q58)
       deallocate(B2)
C
       call sum_stripe(2,shape(X21),size(X21),'21',-1.000,
     & X21,Q58)
       deallocate(Q58)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q56(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q53,B2,Q56)
       deallocate(B2)
C
       call sum_stripe(2,shape(X19),size(X19),'21',-1.000,
     & X19,Q56)
       deallocate(Q56)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S503(M1+1:N1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S503)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S503),size(S503),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'3241',S503,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S507(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S507)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X76),size(X76),'3124',-1.000,
     & X76,S507)
       deallocate(S507)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z411(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X76,F2,Z411)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z411)
       deallocate(Z411)
       deallocate(X76)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S503),size(S503),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'3241',S503,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S509(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S509)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X78),size(X78),'3124',-1.000,
     & X78,S509)
       deallocate(S509)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z413(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X78,F2,Z413)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z413)
       deallocate(Z413)
       deallocate(X78)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S503),size(S503),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,M1-M1/),'3241',S503,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S504(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S504)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X74),size(X74),'3124',-1.000,
     & X74,S504)
       deallocate(S504)
C
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z406(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,X74,F2,Z406)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z406)
       deallocate(Z406)
       deallocate(X74)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S505(M1+1:N1,M1+1:N2,N0+1:N1,N2+1:M2))
       I1=K0*K1*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S505)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S505),size(S505),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'3241',S505,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S506(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S506)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X75),size(X75),'3124',-1.000,
     & X75,S506)
       deallocate(S506)
C
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z407(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,X75,F2,Z407)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234516',
     & 1.000,V3A,Z407)
       deallocate(Z407)
       deallocate(X75)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S505),size(S505),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'3241',S505,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S510(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S510)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X79),size(X79),'3124',-1.000,
     & X79,S510)
       deallocate(S510)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z414(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X79,F2,Z414)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z414)
       deallocate(Z414)
       deallocate(X79)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S505),size(S505),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,M1-M1/),'3241',S505,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S508(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S508)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X77),size(X77),'3124',-1.000,
     & X77,S508)
       deallocate(S508)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z412(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X77,F2,Z412)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134526',
     & -1.000,V3A,Z412)
       deallocate(Z412)
       deallocate(X77)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S511(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S511)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S511),size(S511),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'3241',S511,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S519(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S519)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X84),size(X84),'3124',-1.000,
     & X84,S519)
       deallocate(S519)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z465(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X84,F2,Z465)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z465)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z465)
       deallocate(Z465)
       deallocate(X84)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S511),size(S511),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'3241',S511,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S523(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S523)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X88),size(X88),'3124',-1.000,
     & X88,S523)
       deallocate(S523)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z469(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X88,F2,Z469)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z469)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z469)
       deallocate(Z469)
       deallocate(X88)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S511),size(S511),shape(D1),size(D1),
     & (/N0-N0,N0-N0,M2-M2,N0-N0/),'3241',S511,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S512(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S512)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X80),size(X80),'3124',-1.000,
     & X80,S512)
       deallocate(S512)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z455(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K6*K5
       call EGEMM(I1,I2,I3,X80,F2,Z455)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z455)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z455)
       deallocate(Z455)
       deallocate(X80)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S517(N0+1:M1,M1+1:N2,N0+1:N1,N2+1:M2))
       I1=K0*K1*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S517)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S517),size(S517),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,N0-N0/),'3241',S517,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S522(N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S522)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X87),size(X87),'3124',-1.000,
     & X87,S522)
       deallocate(S522)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z468(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X87,F2,Z468)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z468)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z468)
       deallocate(Z468)
       deallocate(X87)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S517),size(S517),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,N0-N0/),'3241',S517,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S526(N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S526)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X91),size(X91),'3124',-1.000,
     & X91,S526)
       deallocate(S526)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z472(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X91,F2,Z472)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z472)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z472)
       deallocate(Z472)
       deallocate(X91)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S517),size(S517),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,N0-N0/),'3241',S517,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S518(M2+1:N3,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S518)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X83),size(X83),'3124',-1.000,
     & X83,S518)
       deallocate(S518)
C
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z458(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,X83,F2,Z458)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z458)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z458)
       deallocate(Z458)
       deallocate(X83)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S515(N0+1:M1,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S515)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S515),size(S515),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'3241',S515,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S516(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S516)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X82),size(X82),'3124',-1.000,
     & X82,S516)
       deallocate(S516)
C
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z457(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K0*K5
       call EGEMM(I1,I2,I3,X82,F2,Z457)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z457)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z457)
       deallocate(Z457)
       deallocate(X82)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S515),size(S515),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'3241',S515,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S525(N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S525)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X90),size(X90),'3124',-1.000,
     & X90,S525)
       deallocate(S525)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z471(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X90,F2,Z471)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z471)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z471)
       deallocate(Z471)
       deallocate(X90)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S515),size(S515),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N2-N2,N0-N0/),'3241',S515,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S521(N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S521)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X86),size(X86),'3124',-1.000,
     & X86,S521)
       deallocate(S521)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z467(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X86,F2,Z467)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z467)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z467)
       deallocate(Z467)
       deallocate(X86)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S513(N0+1:M1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S513)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S513),size(S513),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'3241',S513,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S514(M2+1:N3,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S514)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X81),size(X81),'3124',-1.000,
     & X81,S514)
       deallocate(S514)
C
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z456(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,X81,F2,Z456)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234615',
     & -1.000,V3A,Z456)
       call sum_stripe(6,shape(V3A),size(V3A),'235614',
     & 1.000,V3A,Z456)
       deallocate(Z456)
       deallocate(X81)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S513),size(S513),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'3241',S513,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S520(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S520)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X85),size(X85),'3124',-1.000,
     & X85,S520)
       deallocate(S520)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z466(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X85,F2,Z466)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134625',
     & 1.000,V3A,Z466)
       call sum_stripe(6,shape(V3A),size(V3A),'135624',
     & -1.000,V3A,Z466)
       deallocate(Z466)
       deallocate(X85)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S513),size(S513),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'3241',S513,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S524(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S524)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X89),size(X89),'3124',-1.000,
     & X89,S524)
       deallocate(S524)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z470(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X89,F2,Z470)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z470)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z470)
       deallocate(Z470)
       deallocate(X89)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q59(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(Q59),size(Q59),shape(B1),size(B1),
     & (/N1-N1,M1-M1/),'21',Q59,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(Q63(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q63)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X15),size(X15),'21',1.000,
     & X15,Q63)
       deallocate(Q63)
C
       call sumx_sorted21(N0,N1,N0,N1,
     & M1,N1,N0,M1,X15,FAHH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M2-N1,N1-N1,N1-N1,N0-N0,M1-M1/),'512346',t3A,F2)
       allocate(Z15(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K5*K9*K9*K6
       I3=K7
       call EGEMM(I1,I2,I3,X15,F2,Z15)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'123465',
     & -1.000,V3A,Z15)
       call sum_stripe(6,shape(V3A),size(V3A),'123564',
     & 1.000,V3A,Z15)
       deallocate(Z15)
       deallocate(X15)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(Q59),size(Q59),shape(B1),size(B1),
     & (/N1-N1,M1-M1/),'21',Q59,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(Q60(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q60)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X13),size(X13),'21',1.000,
     & X13,Q60)
       deallocate(Q60)
C
       call sumx_sorted21(N0,N1,N0,N1,
     & M1,N1,M1,N1,X13,FAHH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N1-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(Z13(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K5*K9*K9*K6
       I3=K7
       call EGEMM(I1,I2,I3,X13,F2,Z13)
       deallocate(F2)
C
       V3A=V3A-Z13
       deallocate(Z13)
       deallocate(X13)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q61(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(Q61),size(Q61),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q61,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(Q62(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q62)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X14),size(X14),'21',1.000,
     & X14,Q62)
       deallocate(Q62)
C
       call sumx_sorted21(N0,N1,N0,N1,
     & N0,M1,N0,M1,X14,FAHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M2-N1,N1-N1,N1-N1,N0-N0,M1-M1/),'412356',t3A,F2)
       allocate(Z14(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K5*K9*K9*K6
       I3=K5
       call EGEMM(I1,I2,I3,X14,F2,Z14)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'123465',
     & 1.000,V3A,Z14)
       call sum_stripe(6,shape(V3A),size(V3A),'123564',
     & -1.000,V3A,Z14)
       deallocate(Z14)
       deallocate(X14)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q64(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q64)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q68(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q64,B2,Q68)
       deallocate(B2)
C
       call sum_stripe(2,shape(X18),size(X18),'21',-1.000,
     & X18,Q68)
       deallocate(Q68)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & M2,N3,N1,M2,X18,FAPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M2-N1,M2-N1,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z18(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K5*K9*K6
       I3=K6
       call EGEMM(I1,I2,I3,X18,F2,Z18)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134562',
     & -1.000,V3A,Z18)
       deallocate(Z18)
       deallocate(X18)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q70(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q64,B2,Q70)
       deallocate(B2)
C
       call sum_stripe(2,shape(X20),size(X20),'21',-1.000,
     & X20,Q70)
       deallocate(Q70)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & M2,N3,N1,M2,X20,FAPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M2-N1,M2-N1,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z20(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K5*K9*K6
       I3=K6
       call EGEMM(I1,I2,I3,X20,F2,Z20)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124563',
     & 1.000,V3A,Z20)
       deallocate(Z20)
       deallocate(X20)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q65(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q64,B2,Q65)
       deallocate(B2)
C
       call sum_stripe(2,shape(X16),size(X16),'21',-1.000,
     & X16,Q65)
       deallocate(Q65)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & M2,N3,M2,N3,X16,FAPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M2-N1,N1-N1,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z16(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K5*K9*K9
       I3=K6
       call EGEMM(I1,I2,I3,X16,F2,Z16)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234561',
     & 1.000,V3A,Z16)
       deallocate(Z16)
       deallocate(X16)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q66(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q66)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Q67(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q66,B2,Q67)
       deallocate(B2)
C
       call sum_stripe(2,shape(X17),size(X17),'21',-1.000,
     & X17,Q67)
       deallocate(Q67)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & N1,M2,M2,N3,X17,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N1-N1,N1-N1,N1-N1,N0-N0,N0-N0,M1-M1/),'123456',t3A,F2)
       allocate(Z17(N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K5*K9*K9
       I3=K9
       call EGEMM(I1,I2,I3,X17,F2,Z17)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234561',
     & 1.000,V3A,Z17)
       deallocate(Z17)
       deallocate(X17)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q71(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q66,B2,Q71)
       deallocate(B2)
C
       call sum_stripe(2,shape(X21),size(X21),'21',-1.000,
     & X21,Q71)
       deallocate(Q71)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & N1,M2,N1,M2,X21,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N1-N1,M2-N1,N1-N1,N0-N0,N0-N0,M1-M1/),'213456',t3A,F2)
       allocate(Z21(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K5*K9*K6
       I3=K9
       call EGEMM(I1,I2,I3,X21,F2,Z21)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124563',
     & -1.000,V3A,Z21)
       deallocate(Z21)
       deallocate(X21)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q69(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q66,B2,Q69)
       deallocate(B2)
C
       call sum_stripe(2,shape(X19),size(X19),'21',-1.000,
     & X19,Q69)
       deallocate(Q69)
C
       call sumx_sorted12(N1,N3,N1,N3,
     & N1,M2,N1,M2,X19,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N1-N1,M2-N1,N1-N1,N0-N0,N0-N0,M1-M1/),'213456',t3A,F2)
       allocate(Z19(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K5*K9*K6
       I3=K9
       call EGEMM(I1,I2,I3,X19,F2,Z19)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134562',
     & 1.000,V3A,Z19)
       deallocate(Z19)
       deallocate(X19)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S527(M1+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S527)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'2341',S527,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S529(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S529)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2314',-0.500,
     & X5,S529)
       deallocate(S529)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'2341',S527,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S530(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S530)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2314',-0.500,
     & X4,S530)
       deallocate(S530)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'3421',S527,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S531(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S531)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',1.000,X3,
     & S531)
       deallocate(S531)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'2431',S527,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S532(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S532)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2314',-1.000,
     & X2,S532)
       deallocate(S532)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,M1-M1/),'2431',S527,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S533(M2+1:N3,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S533)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',-1.000,
     & X1,S533)
       deallocate(S533)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N0-N0,M1-M1/),'4231',S527,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S611(N0+1:M1,N0+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S611)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S611),size(S611),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S611,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S612(M2+1:N3,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S612)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',1.000,X1,
     & S612)
       deallocate(S612)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'3241',S527,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S615(N1+1:M2,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S615)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S615),size(S615),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2314',S615,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S616(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S616)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',-1.000,
     & X4,S616)
       deallocate(S616)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'3241',S527,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S617(N1+1:M2,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S617)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S617),size(S617),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2314',S617,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S618(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S618)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',-1.000,
     & X5,S618)
       deallocate(S618)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder_shift(4,shape(S527),size(S527),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,M1-M1/),'2341',S527,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(S528(N1+1:M2,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K9
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S528)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2314',-0.500,
     & X6,S528)
       deallocate(S528)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S611),size(S611),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'2314',S611,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S613(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S613)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',1.000,X2,
     & S613)
       deallocate(S613)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S611),size(S611),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3214',S611,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S614(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S614)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',-1.000,
     & X3,S614)
       deallocate(S614)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S617),size(S617),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'2314',S617,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S619(N1+1:M2,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S619)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'2134',-1.000,
     & X6,S619)
       deallocate(S619)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S534(N0+1:M1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S534)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S534),size(S534),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'2431',S534,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S542(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S542)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2413',1.000,X2,
     & S542)
       deallocate(S542)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S534),size(S534),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'3421',S534,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S543(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S543)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2413',-1.000,
     & X3,S543)
       deallocate(S543)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S534),size(S534),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S534,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(S535(N1+1:M2,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K9
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S535)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'2314',-0.500,
     & X12,S535)
       deallocate(S535)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S534),size(S534),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S534,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S536(M2+1:N3,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S536)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'2314',-0.500,
     & X11,S536)
       deallocate(S536)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S534),size(S534),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'2341',S534,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,M2-N1,N1-N1/),'3412',t2A,D2)
       allocate(S537(M2+1:N3,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S537)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2314',-0.500,
     & X10,S537)
       deallocate(S537)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S534),size(S534),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'3421',S534,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S538(N1+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S538)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X67),size(X67),'2314',1.000,
     & X67,S538)
       deallocate(S538)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S534),size(S534),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'2431',S534,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S539(N1+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S539)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X66),size(X66),'2314',-1.000,
     & X66,S539)
       deallocate(S539)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S534),size(S534),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'2431',S534,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S540(M2+1:N3,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S540)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2413',1.000,X1,
     & S540)
       deallocate(S540)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S534),size(S534),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N0-N0,N0-N0/),'4231',S534,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S620(N0+1:M1,N0+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S620)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S620),size(S620),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S620,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S621(M2+1:N3,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S621)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',1.000,X7,
     & S621)
       deallocate(S621)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S534),size(S534),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'3241',S534,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S624(N1+1:M2,N0+1:N1,N1+1:N3,N0+1:M1))
       I1=K5*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S624)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S624),size(S624),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2314',S624,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S625(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S625)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',-1.000,
     & X10,S625)
       deallocate(S625)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S534),size(S534),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N1-N1,N0-N0/),'3241',S534,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S626(N1+1:M2,N0+1:N1,N1+1:N3,N0+1:M1))
       I1=K5*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S626)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S626),size(S626),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2314',S626,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S627(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S627)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'2134',-1.000,
     & X11,S627)
       deallocate(S627)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S534),size(S534),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'2431',S534,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S541(M2+1:N3,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S541)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X65),size(X65),'2314',-1.000,
     & X65,S541)
       deallocate(S541)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S620),size(S620),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S620,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S622(N1+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S622)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X8),size(X8),'2134',1.000,X8,
     & S622)
       deallocate(S622)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S620),size(S620),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3214',S620,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S623(N1+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S623)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',-1.000,
     & X9,S623)
       deallocate(S623)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S626),size(S626),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'2314',S626,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S628(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S628)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'2134',-1.000,
     & X12,S628)
       deallocate(S628)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(S561(N0+1:M1,N0+1:M1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K5*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S561)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(S561),size(S561),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3412',S561,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S562(M2+1:N3,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S562)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',-0.500,
     & X7,S562)
       deallocate(S562)
C
       call sumx_sorted2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M2,N3,N0,M1,N0,M1,X7,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z7(N1+1:M2,N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5*K5*K6
       I2=K7*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,X7,D2,Z7)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'236145',
     & -1.000,V3A,Z7)
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4132',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S563(N1+1:M2,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S563)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S563),size(S563),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'3412',S563,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S564(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S564)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',1.000,X4,
     & S564)
       deallocate(S564)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S565(N1+1:M2,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S565)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S565),size(S565),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'3412',S565,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S566(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S566)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',-1.000,
     & X5,S566)
       deallocate(S566)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S553(N1+1:M2,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S553)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S553),size(S553),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N1-N1/),'2341',S553,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S568(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S568)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2413',1.000,X4,
     & S568)
       deallocate(S568)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S553),size(S553),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N1-N1/),'2431',S553,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S573(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S573)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'3412',1.000,X6,
     & S573)
       deallocate(S573)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S553),size(S553),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N1-N1/),'2341',S553,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S554(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S554)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2413',1.000,
     & X10,S554)
       deallocate(S554)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(S553),size(S553),shape(D1),size(D1),
     & (/N1-N1,N1-N1,N0-N0,N1-N1/),'3421',S553,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(S567(N0+1:M1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K5*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S567)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'3412',0.500,X8,
     & S567)
       deallocate(S567)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S556(N1+1:M2,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S556)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder_shift(4,shape(S556),size(S556),shape(D1),size(D1),
     & (/N1-N1,N1-N1,N0-N0,N1-N1/),'3421',S556,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(S569(N0+1:M1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K5*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S569)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'3412',0.500,X9,
     & S569)
       deallocate(S569)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S556),size(S556),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N1-N1/),'2431',S556,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S574(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S574)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2413',-1.000,
     & X6,S574)
       deallocate(S574)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S556),size(S556),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N1-N1/),'2341',S556,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,N0-N0/),'3124',t2A,D2)
       allocate(S557(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S557)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'2413',1.000,
     & X11,S557)
       deallocate(S557)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S556),size(S556),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N1-N1/),'2341',S556,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,M2-N1,M1-N0/),'3124',t2A,D2)
       allocate(S571(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S571)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2413',1.000,X5,
     & S571)
       deallocate(S571)
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
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S552(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,Q72,D2,S552)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',-1.000,
     & X12,S552)
       deallocate(S552)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q72),size(Q72),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q72,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(S570(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S570)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S570)
       deallocate(S570)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q72),size(Q72),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q72,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(S572(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S572)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S572)
       deallocate(S572)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q72),size(Q72),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q72,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',t2A,D2)
       allocate(S558(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S558)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S558)
       deallocate(S558)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q72),size(Q72),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q72,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',t2A,D2)
       allocate(S560(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S560)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S560)
       deallocate(S560)
C
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S575(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,Q72,D2,S575)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S575)
       deallocate(S575)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S580(N0+1:M1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S580)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S580),size(S580),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S580,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S594(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S594)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2413',-1.000,
     & X3,S594)
       deallocate(S594)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S580),size(S580),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S580,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S595(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S595)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2413',-1.000,
     & X2,S595)
       deallocate(S595)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S580),size(S580),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S580,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S596(M2+1:N3,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S596)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2413',-1.000,
     & X1,S596)
       deallocate(S596)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S580),size(S580),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S580,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S581(N1+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S581)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X67),size(X67),'2314',1.000,
     & X67,S581)
       deallocate(S581)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z94(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K9
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X67,D2,Z94)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'126345',
     & -1.000,V3A,Z94)
       call sum_stripe(6,shape(V3A),size(V3A),'126354',
     & 1.000,V3A,Z94)
       deallocate(Z94)
       deallocate(X67)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S580),size(S580),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S580,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S582(N1+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S582)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X66),size(X66),'2314',1.000,
     & X66,S582)
       deallocate(S582)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z93(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K9
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X66,D2,Z93)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'136245',
     & 1.000,V3A,Z93)
       call sum_stripe(6,shape(V3A),size(V3A),'136254',
     & -1.000,V3A,Z93)
       deallocate(Z93)
       deallocate(X66)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder_shift(4,shape(S580),size(S580),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S580,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S583(M2+1:N3,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S583)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X65),size(X65),'2314',1.000,
     & X65,S583)
       deallocate(S583)
C
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z92(N1+1:M2,N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5*K5*K6
       I2=K7*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,X65,D2,Z92)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'236145',
     & -1.000,V3A,Z92)
       call sum_stripe(6,shape(V3A),size(V3A),'236154',
     & 1.000,V3A,Z92)
       deallocate(Z92)
       deallocate(X65)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S592(N1+1:M2,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S592)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S592),size(S592),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N1-N1/),'2341',S592,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S602(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S602)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2413',1.000,X6,
     & S602)
       deallocate(S602)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S592),size(S592),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N1-N1/),'2341',S592,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S604(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S604)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2413',1.000,X5,
     & S604)
       deallocate(S604)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S592),size(S592),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N1-N1/),'2341',S592,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S593(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S593)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X11),size(X11),'2413',1.000,
     & X11,S593)
       deallocate(S593)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S584(N1+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S584)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S584),size(S584),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3412',S584,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S585(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S585)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'2134',-1.000,
     & X11,S585)
       deallocate(S585)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S584),size(S584),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3412',S584,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S588(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S588)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'2134',-1.000,
     & X12,S588)
       deallocate(S588)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S599(N1+1:M2,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S599)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S599),size(S599),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'3412',S599,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S600(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S600)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',-1.000,
     & X4,S600)
       deallocate(S600)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S590(N1+1:M2,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S590)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S590),size(S590),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N1-N1/),'2341',S590,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,M1-N0/),'3124',t2B,D2)
       allocate(S603(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S603)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'2413',1.000,X4,
     & S603)
       deallocate(S603)
C
       call sumx_sorted2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M2,N3,N1,M2,M1,N1,X4,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(Z4(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K6
       I2=K5*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,X4,D2,Z4)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'345126',
     & -1.000,V3A,Z4)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S590),size(S590),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N1-N1/),'2341',S590,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S591(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S591)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X10),size(X10),'2413',1.000,
     & X10,S591)
       deallocate(S591)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S590),size(S590),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N1-N1/),'2341',S590,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S601(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S601)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'3412',-1.000,
     & X6,S601)
       deallocate(S601)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S586(N1+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S586)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S586),size(S586),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3412',S586,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S589(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S589)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'3124',1.000,
     & X12,S589)
       deallocate(S589)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S586),size(S586),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3412',S586,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S587(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S587)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',-1.000,
     & X10,S587)
       deallocate(S587)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q73(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q73),size(Q73),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q73,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',t2A,D2)
       allocate(S607(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S607)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2341',-1.000,
     & X3,S607)
       deallocate(S607)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q73),size(Q73),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q73,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(S608(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S608)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-1.000,
     & X9,S608)
       deallocate(S608)
C
       call sumx_sorted2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,M2,N0,M1,N0,M1,X9,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z9(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K9
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X9,D2,Z9)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'126345',
     & -1.000,V3A,Z9)
       deallocate(Z9)
       deallocate(X9)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q73),size(Q73),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q73,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(S609(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S609)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2341',-1.000,
     & X8,S609)
       deallocate(S609)
C
       call sumx_sorted2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,M2,N0,M1,N0,M1,X8,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(Z8(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K9
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X8,D2,Z8)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'136245',
     & 1.000,V3A,Z8)
       deallocate(Z8)
       deallocate(X8)
C
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,M1-N0/),'3124',t2A,D2)
       allocate(S610(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,Q73,D2,S610)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'2341',-1.000,
     & X6,S610)
       deallocate(S610)
C
       call sumx_sorted2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,M2,N1,M2,M1,N1,X6,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,M2-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(Z6(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N1))
       I1=K7*K9*K9
       I2=K5*K5*K6
       I3=K3
       call EGEMM(I1,I2,I3,X6,D2,Z6)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'145236',
     & -1.000,V3A,Z6)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S605(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,Q73,D2,S605)
       deallocate(D2)
C
       call sum_stripe(4,shape(X12),size(X12),'2341',-1.000,
     & X12,S605)
       deallocate(S605)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder_shift(2,shape(Q73),size(Q73),shape(B1),size(B1),
     & (/N1-N1,N0-N0/),'21',Q73,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',t2A,D2)
       allocate(S606(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S606)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2341',-1.000,
     & X2,S606)
       deallocate(S606)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4132',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S546(N1+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S546)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S546),size(S546),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3412',S546,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S551(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S551)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'3124',-1.000,
     & X12,S551)
       deallocate(S551)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S546),size(S546),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3412',S546,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S547(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S547)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',1.000,
     & X10,S547)
       deallocate(S547)
C
       call sumx_sorted2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M2,N3,N1,M2,N0,M1,X10,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',t2A,D2)
       allocate(Z10(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K6
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,X10,D2,Z10)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'346125',
     & 1.000,V3A,Z10)
       call sum_stripe(6,shape(V3A),size(V3A),'356124',
     & -1.000,V3A,Z10)
       deallocate(Z10)
       deallocate(X10)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S544(N1+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S544)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S544),size(S544),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3412',S544,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S545(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S545)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X11),size(X11),'2134',-1.000,
     & X11,S545)
       deallocate(S545)
C
       call sumx_sorted2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M2,N3,N1,M2,N0,M1,X11,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',t2A,D2)
       allocate(Z11(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K6
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,X11,D2,Z11)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'246135',
     & -1.000,V3A,Z11)
       call sum_stripe(6,shape(V3A),size(V3A),'256134',
     & 1.000,V3A,Z11)
       deallocate(Z11)
       deallocate(X11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S544),size(S544),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3412',S544,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S550(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S550)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X12),size(X12),'2134',-1.000,
     & X12,S550)
       deallocate(S550)
C
       call sumx_sorted2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,M2,N1,M2,N0,M1,X12,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,M2-N1,N0-N0,M1-N0/),'1234',t2A,D2)
       allocate(Z12(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K9
       I2=K7*K5*K6
       I3=K3
       call EGEMM(I1,I2,I3,X12,D2,Z12)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'146235',
     & 1.000,V3A,Z12)
       call sum_stripe(6,shape(V3A),size(V3A),'156234',
     & -1.000,V3A,Z12)
       deallocate(Z12)
       deallocate(X12)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,M1-N0/),'12',t1A,B2)
       allocate(S576(M1+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S576)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S576),size(S576),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S576,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S578(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S578)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2314',1.000,X2,
     & S578)
       deallocate(S578)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S576),size(S576),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S576,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,M2-N1,N0-N0/),'3124',t2B,D2)
       allocate(S579(M2+1:N3,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S579)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2314',1.000,X1,
     & S579)
       deallocate(S579)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder_shift(4,shape(S576),size(S576),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,M1-M1/),'2431',S576,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S577(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S577)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',1.000,X3,
     & S577)
       deallocate(S577)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,M1-N0/),'1234',t2A,D2)
       allocate(S548(N0+1:M1,M1+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S548)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S548),size(S548),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3412',S548,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S549(M2+1:N3,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S549)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',-0.500,
     & X1,S549)
       deallocate(S549)
C
       call sumx_sorted2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M2,N3,N0,M1,M1,N1,X1,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(Z1(N1+1:M2,N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1,M1+1:N1))
       I1=K7*K5*K6
       I2=K5*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,X1,D2,Z1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'234156',
     & -1.000,V3A,Z1)
       call sum_stripe(6,shape(V3A),size(V3A),'235146',
     & 1.000,V3A,Z1)
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S548),size(S548),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'4312',S548,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S559(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S559)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'2134',0.500,X3,
     & S559)
       deallocate(S559)
C
       call sumx_sorted2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,M2,N0,M1,M1,N1,X3,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(Z3(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K9
       I2=K5*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X3,D2,Z3)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124356',
     & -1.000,V3A,Z3)
       call sum_stripe(6,shape(V3A),size(V3A),'125346',
     & 1.000,V3A,Z3)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder_shift(4,shape(S548),size(S548),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,M1-M1/),'3412',S548,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S555(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S555)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-0.500,
     & X2,S555)
       deallocate(S555)
C
       call sumx_sorted2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,M2,N0,M1,M1,N1,X2,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,M2-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(Z2(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K9
       I2=K5*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X2,D2,Z2)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'134256',
     & 1.000,V3A,Z2)
       call sum_stripe(6,shape(V3A),size(V3A),'135246',
     & -1.000,V3A,Z2)
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,M1-N0/),'3124',t2B,D2)
       allocate(S597(N1+1:M2,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S597)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(S597),size(S597),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,M1-M1/),'3412',S597,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(S598(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S598)
       deallocate(D1)
       deallocate(B2)
       deallocate(S597)
C
       call sum_stripe(4,shape(X5),size(X5),'2134',-1.000,
     & X5,S598)
       deallocate(S598)
C
       call sumx_sorted2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M2,N3,N1,M2,M1,N1,X5,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N1-N1,N1-N1,N0-N0,N0-N0/),'1234',t2A,D2)
       allocate(Z5(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K6
       I2=K5*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,X5,D2,Z5)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'245136',
     & 1.000,V3A,Z5)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,M1-N0/),'1324',VBHPPH,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z57(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z57)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z57)
       deallocate(Z57)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,M1-N0/),'1324',VBHPPH,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z58(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z58)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124536',
     & 1.000,V3A,Z58)
       deallocate(Z58)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,M2-N2,N1-N1,N0-N0/),'1324',VBHPPH,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z67(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z67)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z67)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z67)
       deallocate(Z67)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'1324',VBHPPH,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z68(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z68)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z68)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z68)
       deallocate(Z68)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'1324',VBHPPH,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z69(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z69)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z69)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z69)
       deallocate(Z69)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHPPH),size(VBHPPH),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'1324',VBHPPH,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,N2-N2,M2-N1,N1-N1,N0-N0,M1-M1/),'412356',t3B1,F2)
       allocate(Z70(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z70)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(6,shape(V3A),size(V3A),'124635',
     & -1.000,V3A,Z70)
       call sum_stripe(6,shape(V3A),size(V3A),'125634',
     & 1.000,V3A,Z70)
       deallocate(Z70)
C
       call sumx_sorted3(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,M1,N1,HT3A,V3A,1.0)
       deallocate(V3A)
C
       end
