       subroutine t2C_update_opt(N0,N1,N2,N3,V2C,shift,
     & K1,K2,K3,K4,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & t1A,t1B,t2A,t2B,t2C)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 shift
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
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
C
       real*8 V2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U3(:,:,:,:)
       real*8,allocatable::X3(:,:)
       real*8,allocatable::U4(:,:,:,:)
       real*8,allocatable::X4(:,:)
       real*8,allocatable::U5(:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::U11(:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U13(:,:,:,:)
       real*8,allocatable::U21(:,:,:,:)
       real*8,allocatable::U45(:,:,:,:)
       real*8,allocatable::U41(:,:,:,:)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q1(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N2,N0+1:N2))
       X3=0.0d0
       X3=X3+Q1
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N2-N2,N2-N2/),'4213',VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q2(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N2+1:N3,N2+1:N3))
       X4=0.0d0
       X4=X4+Q2
       deallocate(Q2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(VCHHHH),size(VCHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,N0-N0/),'4312',VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S10(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X7=0.0d0
       call sum_stripe(4,shape(X7),size(X7),'2134',1.000,X7,
     & S10)
       deallocate(S10)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHPHP),size(VCHPHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N0-N0/),'1432',VCHPHP,D1)
       allocate(S12(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S12)
       deallocate(D1)
C
       allocate(X8(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X8=0.0d0
       call sum_stripe(4,shape(X8),size(X8),'3124',1.000,X8,
     & S12)
       deallocate(S12)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(VBHHPH),size(VBHHPH),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4231',VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S14(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S14)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2314',1.000,X8,
     & S14)
       deallocate(S14)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHPPP),size(VBHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N1-N1,N2-N2/),'1423',VBHPPP,D1)
       allocate(S16(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       I1=K4*K3*K1
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S16)
       deallocate(D1)
C
       allocate(X2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       X2=0.0d0
       call sum_stripe(4,shape(X2),size(X2),'4123',1.000,X2,
     & S16)
       deallocate(S16)
C
       allocate(Q3(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,FBHP,t1B,Q3)
C
       call sum_stripe(2,shape(X3),size(X3),'21',1.000,X3,
     & Q3)
       deallocate(Q3)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(FBHP),size(FBHP),shape(B1),size(B1),
     & (/N0-N0,N2-N2/),'21',FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q4(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q4)
       deallocate(B1)
       deallocate(B2)
C
       call sum_stripe(2,shape(X4),size(X4),'21',-1.000,X4,
     & Q4)
       deallocate(Q4)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N0-N0/),'1342',VCHHHP,D1)
       allocate(S20(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S20)
       deallocate(D1)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(S20),size(S20),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S20,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(U21(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,U21)
       deallocate(D1)
       deallocate(D2)
C
       V2C=V2C+0.500*U21
       call sum_stripe(4,shape(V2C),size(V2C),'1243',-0.500,
     & V2C,U21)
       deallocate(U21)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(S20),size(S20),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3214',S20,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S44(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(S44),size(S44),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2134',S44,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(U45(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,U45)
       deallocate(D1)
       deallocate(B2)
C
       V2C=V2C+U45
       call sum_stripe(4,shape(V2C),size(V2C),'1243',-1.000,
     & V2C,U45)
       deallocate(U45)
       deallocate(S44)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q5(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3+Q5
       deallocate(Q5)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N2-N2,N2-N2/),'1423',VCHPPP,D1)
       allocate(S25(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S25)
       deallocate(D1)
C
       allocate(X6(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       X6=0.0d0
       call sum_stripe(4,shape(X6),size(X6),'4123',-1.000,
     & X6,S25)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(S25),size(S25),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,N0-N0/),'3241',S25,D1)
       allocate(S46(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S46)
       deallocate(D1)
C
       allocate(X1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X1=0.0d0
       call sum_stripe(4,shape(X1),size(X1),'3124',-1.000,
     & X1,S46)
       deallocate(S46)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N2-N2/),'4123',VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q6(N2+1:N3,N2+1:N3))
       I1=K4*K4
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       X4=X4-Q6
       deallocate(Q6)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S30(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S30)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'3412',0.500,X2,
     & S30)
       deallocate(S30)
C
       call sum_stripe(4,shape(X2),size(X2),'4231',1.000,X2,
     & VBHPPH)
C
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(U3(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,X2,D2,U3)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2C),size(V2C),'2314',-1.000,
     & V2C,U3)
       call sum_stripe(4,shape(V2C),size(V2C),'1324',1.000,
     & V2C,U3)
       call sum_stripe(4,shape(V2C),size(V2C),'2413',1.000,
     & V2C,U3)
       call sum_stripe(4,shape(V2C),size(V2C),'1423',-1.000,
     & V2C,U3)
       deallocate(U3)
       deallocate(X2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N2-N2,N0-N0/),'4213',t2B,D2)
       allocate(S32(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S32)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'3412',-1.000,
     & X6,S32)
C
       call sum_stripe(4,shape(X6),size(X6),'2431',1.000,X6,
     & VCHPHP)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(U7(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,X6,D2,U7)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2C),size(V2C),'2314',1.000,
     & V2C,U7)
       call sum_stripe(4,shape(V2C),size(V2C),'1324',-1.000,
     & V2C,U7)
       call sum_stripe(4,shape(V2C),size(V2C),'2413',-1.000,
     & V2C,U7)
       call sum_stripe(4,shape(V2C),size(V2C),'1423',1.000,
     & V2C,U7)
       deallocate(U7)
       deallocate(X6)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(S32),size(S32),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,N0-N0/),'4312',S32,D1)
       allocate(S48(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S48)
       deallocate(D1)
C
       call sum_stripe(4,shape(X8),size(X8),'4123',1.000,X8,
     & S48)
       deallocate(S48)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N2-N2/),'3421',t2B,D2)
       allocate(Q8(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q8)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X4),size(X4),'21',-1.000,X4,
     & Q8)
       deallocate(Q8)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N2-N2/),'3421',VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(Q9(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q9)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X4),size(X4),'21',0.500,X4,
     & Q9)
       deallocate(Q9)
C
       allocate(S37(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K4*K4
       call EGEMM(I1,I2,I3,VCHHPP,t2C,S37)
C
       allocate(X5(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       X5=0.0d0
       call sum_stripe(4,shape(X5),size(X5),'3412',0.500,X5,
     & S37)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(S37),size(S37),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'4312',S37,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S56(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',0.500,X7,
     & S56)
       deallocate(S56)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S40(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S40)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(S40),size(S40),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3412',S40,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(U41(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,U41)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2C),size(V2C),'1423',-1.000,
     & V2C,U41)
       call sum_stripe(4,shape(V2C),size(V2C),'1324',1.000,
     & V2C,U41)
       deallocate(U41)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(S40),size(S40),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N2-N2,N0-N0/),'4312',S40,D1)
       allocate(S53(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S53)
       deallocate(D1)
C
       call sum_stripe(4,shape(X8),size(X8),'4123',1.000,X8,
     & S53)
       deallocate(S53)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q11(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(Q11),size(Q11),shape(B1),size(B1),
     & (/N2-N2,N0-N0/),'21',Q11,B1)
       allocate(Q12(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,B1,t1B,Q12)
       deallocate(B1)
C
       call sum_stripe(2,shape(X3),size(X3),'21',1.000,X3,
     & Q12)
       deallocate(Q12)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q13(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,Q11,B2,Q13)
       deallocate(B2)
C
       call sum_stripe(2,shape(X4),size(X4),'21',-1.000,X4,
     & Q13)
       deallocate(Q13)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N0-N0,N2-N2/),'1243',VCHPPP,D1)
       allocate(S27(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K2
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,t2C,S27)
       deallocate(D1)
C
       call sum_stripe(4,shape(X1),size(X1),'3412',0.500,X1,
     & S27)
       deallocate(S27)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',1.000,X1,
     & VCHHHP)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(U1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,X1,B2,U1)
       deallocate(B2)
C
       call sum_stripe(4,shape(V2C),size(V2C),'2134',-1.000,
     & V2C,U1)
       V2C=V2C+U1
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'4123',t2B,D2)
       allocate(Q7(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q7)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X3),size(X3),'21',1.000,X3,
     & Q7)
       deallocate(Q7)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N2-N2,N0-N0,N0-N0,N2-N2/),'2341',VCHHPP,D1)
       allocate(S50(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S50)
       deallocate(D1)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(S50),size(S50),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S50,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q14(N0+1:N2,N0+1:N2))
       I1=K2*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3+Q14
       deallocate(Q14)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(S50),size(S50),shape(D1),size(D1),
     & (/N2-N2,N0-N0,N0-N0,N0-N0/),'4231',S50,D1)
       allocate(S51(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,S51)
       deallocate(D1)
C
       call sum_stripe(4,shape(X5),size(X5),'3124',1.000,X5,
     & S51)
C
       call sum_stripe(4,shape(X5),size(X5),'3412',1.000,X5,
     & VCHHHH)
C
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N2-N2/),'3412',t2C,D2)
       allocate(U6(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K2*K2
       call EGEMM(I1,I2,I3,X5,D2,U6)
       deallocate(D2)
C
       V2C=V2C+0.500*U6
       deallocate(U6)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(S51),size(S51),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3214',S51,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(S59(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',1.000,X7,
     & S59)
       deallocate(S59)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(U11(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,X7,B2,U11)
       deallocate(B2)
C
       V2C=V2C+U11
       deallocate(U11)
       deallocate(X7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(Q10(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q10)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X3),size(X3),'21',0.500,X3,
     & Q10)
       deallocate(Q10)
C
       call sum_stripe(2,shape(X3),size(X3),'21',1.000,X3,
     & FBHH)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(U4(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,X3,D2,U4)
       deallocate(D2)
C
       V2C=V2C+U4
       call sum_stripe(4,shape(V2C),size(V2C),'1243',-1.000,
     & V2C,U4)
       deallocate(U4)
       deallocate(X3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q15(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q16(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,Q15,B2,Q16)
       deallocate(B2)
C
       call sum_stripe(2,shape(X4),size(X4),'21',-1.000,X4,
     & Q16)
       deallocate(Q16)
C
       X4=X4+FBPP
C
       allocate(U5(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,X4,t2C,U5)
C
       call sum_stripe(4,shape(V2C),size(V2C),'2341',1.000,
     & V2C,U5)
       call sum_stripe(4,shape(V2C),size(V2C),'1342',-1.000,
     & V2C,U5)
       deallocate(U5)
       deallocate(X4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(VCHHHP),size(VCHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'4132',VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(t2C),size(t2C),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N2-N2,N0-N0/),'3124',t2C,D2)
       allocate(S22(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S22)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2314',-1.000,
     & X8,S22)
       deallocate(S22)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(U13(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,X8,B2,U13)
       deallocate(B2)
C
       call sum_stripe(4,shape(V2C),size(V2C),'2134',-1.000,
     & V2C,U13)
       V2C=V2C+U13
       call sum_stripe(4,shape(V2C),size(V2C),'2143',1.000,
     & V2C,U13)
       call sum_stripe(4,shape(V2C),size(V2C),'1243',-1.000,
     & V2C,U13)
       deallocate(U13)
       deallocate(X8)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(VCHPPP),size(VCHPPP),shape(D1),
     & size(D1),(/N2-N2,N2-N2,N2-N2,N0-N0/),'3124',VCHPPP,D1)
       allocate(U2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,t1B,U2)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2C),size(V2C),'3124',1.000,
     & V2C,U2)
       call sum_stripe(4,shape(V2C),size(V2C),'4123',-1.000,
     & V2C,U2)
       deallocate(U2)
C
       end
