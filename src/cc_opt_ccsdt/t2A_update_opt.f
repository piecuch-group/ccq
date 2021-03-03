       subroutine t2A_update_opt(N0,N1,N2,N3,V2A,shift,
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
       real*8 V2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S60(:,:,:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:)
       real*8,allocatable::X2(:,:)
       real*8,allocatable::U3(:,:,:,:)
       real*8,allocatable::X3(:,:)
       real*8,allocatable::U4(:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::U9(:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U11(:,:,:,:)
       real*8,allocatable::U15(:,:,:,:)
       real*8,allocatable::U43(:,:,:,:)
       real*8,allocatable::U35(:,:,:,:)
       real*8,allocatable::U41(:,:,:,:)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(VAHHHH),size(VAHHHH),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N0-N0,N0-N0/),'4312',VAHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S8(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X7=0.0d0
       call sum_stripe(4,shape(X7),size(X7),'2134',1.000,X7,
     & S8)
       deallocate(S8)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHPHP),size(VAHPHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N0-N0/),'1432',VAHPHP,D1)
       allocate(S10(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S10)
       deallocate(D1)
C
       allocate(X8(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X8=0.0d0
       call sum_stripe(4,shape(X8),size(X8),'3124',1.000,X8,
     & S10)
       deallocate(S10)
C
       allocate(Q1(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,FAHP,t1A,Q1)
C
       allocate(X2(N0+1:N1,N0+1:N1))
       X2=0.0d0
       call sum_stripe(2,shape(X2),size(X2),'21',1.000,X2,
     & Q1)
       deallocate(Q1)
C
       allocate(B1(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(FAHP),size(FAHP),shape(B1),size(B1),
     & (/N0-N0,N1-N1/),'21',FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q2(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q2)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X3(N1+1:N3,N1+1:N3))
       X3=0.0d0
       call sum_stripe(2,shape(X3),size(X3),'21',-1.000,X3,
     & Q2)
       deallocate(Q2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N0-N0/),'1342',VAHHHP,D1)
       allocate(S14(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S14)
       deallocate(D1)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(S14),size(S14),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'2314',S14,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(U15(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K3
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,U15)
       deallocate(D1)
       deallocate(D2)
C
       V2A=V2A+0.500*U15
       call sum_stripe(4,shape(V2A),size(V2A),'1243',-0.500,
     & V2A,U15)
       deallocate(U15)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(S14),size(S14),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3214',S14,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S42(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(S42),size(S42),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'2134',S42,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(U43(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,U43)
       deallocate(D1)
       deallocate(B2)
C
       V2A=V2A+U43
       call sum_stripe(4,shape(V2A),size(V2A),'1243',-1.000,
     & V2A,U43)
       deallocate(U43)
       deallocate(S42)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'3142',VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q3(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q3)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q3
       deallocate(Q3)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N1-N1,N1-N1/),'1423',VAHPPP,D1)
       allocate(S19(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S19)
       deallocate(D1)
C
       allocate(X5(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       X5=0.0d0
       call sum_stripe(4,shape(X5),size(X5),'4123',-1.000,
     & X5,S19)
C
       call sum_stripe(4,shape(X5),size(X5),'2431',1.000,X5,
     & VAHPHP)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(U6(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K3*K1
       call EGEMM(I1,I2,I3,X5,D2,U6)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2314',1.000,
     & V2A,U6)
       call sum_stripe(4,shape(V2A),size(V2A),'1324',-1.000,
     & V2A,U6)
       call sum_stripe(4,shape(V2A),size(V2A),'2413',-1.000,
     & V2A,U6)
       call sum_stripe(4,shape(V2A),size(V2A),'1423',1.000,
     & V2A,U6)
       deallocate(U6)
       deallocate(X5)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(S19),size(S19),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N1-N1,N0-N0/),'3241',S19,D1)
       allocate(S44(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S44)
       deallocate(D1)
C
       allocate(X1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X1=0.0d0
       call sum_stripe(4,shape(X1),size(X1),'3124',-1.000,
     & X1,S44)
       deallocate(S44)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q4(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3-Q4
       deallocate(Q4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S24(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S24)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2314',1.000,X8,
     & S24)
       deallocate(S24)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N2-N2,N1-N1/),'2314',VBPHPP,D1)
       allocate(S26(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S26)
       deallocate(D1)
C
       allocate(X6(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       X6=0.0d0
       call sum_stripe(4,shape(X6),size(X6),'4123',1.000,X6,
     & S26)
       deallocate(S26)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N0-N0/),'3142',VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q5(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q5
       deallocate(Q5)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q6(N1+1:N3,N1+1:N3))
       I1=K3*K3
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       X3=X3+Q6
       deallocate(Q6)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N1-N1,N1-N1/),'3421',VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(Q7(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q7)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X3),size(X3),'21',0.500,X3,
     & Q7)
       deallocate(Q7)
C
       allocate(S31(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K1
       I3=K3*K3
       call EGEMM(I1,I2,I3,VAHHPP,t2A,S31)
C
       allocate(X4(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       X4=0.0d0
       call sum_stripe(4,shape(X4),size(X4),'3412',0.500,X4,
     & S31)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(S31),size(S31),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'4312',S31,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S52(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',0.500,X7,
     & S52)
       deallocate(S52)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'4231',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S34(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K3
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S34)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(S34),size(S34),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3412',S34,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(U35(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,U35)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'1423',-1.000,
     & V2A,U35)
       call sum_stripe(4,shape(V2A),size(V2A),'1324',1.000,
     & V2A,U35)
       deallocate(U35)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(S34),size(S34),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N1-N1,N0-N0/),'4312',S34,D1)
       allocate(S49(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S49)
       deallocate(D1)
C
       call sum_stripe(4,shape(X8),size(X8),'4123',1.000,X8,
     & S49)
       deallocate(S49)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(Q10(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q10)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X2),size(X2),'21',1.000,X2,
     & Q10)
       deallocate(Q10)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S38(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S38)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X6),size(X6),'3412',1.000,X6,
     & S38)
       deallocate(S38)
C
       call sum_stripe(4,shape(X6),size(X6),'1324',1.000,X6,
     & VBHPPH)
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(U7(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K4*K2
       call EGEMM(I1,I2,I3,X6,D2,U7)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2314',-1.000,
     & V2A,U7)
       call sum_stripe(4,shape(V2A),size(V2A),'1324',1.000,
     & V2A,U7)
       call sum_stripe(4,shape(V2A),size(V2A),'2413',1.000,
     & V2A,U7)
       call sum_stripe(4,shape(V2A),size(V2A),'1423',-1.000,
     & V2A,U7)
       deallocate(U7)
       deallocate(X6)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N2-N2/),'4231',VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S40(N1+1:N3,N0+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K1*K3
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S40)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(S40),size(S40),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3412',S40,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(U41(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,U41)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'1423',-1.000,
     & V2A,U41)
       call sum_stripe(4,shape(V2A),size(V2A),'1324',1.000,
     & V2A,U41)
       deallocate(U41)
       deallocate(S40)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(VAHHHP),size(VAHHHP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N0-N0/),'4132',VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(S16(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S16)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2314',-1.000,
     & X8,S16)
       deallocate(S16)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N0-N0,N1-N1/),'1243',VAHPPP,D1)
       allocate(S21(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K1*K1
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,t2A,S21)
       deallocate(D1)
C
       call sum_stripe(4,shape(X1),size(X1),'3412',0.500,X1,
     & S21)
       deallocate(S21)
C
       call sum_stripe(4,shape(X1),size(X1),'2134',1.000,X1,
     & VAHHHP)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(U1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,X1,B2,U1)
       deallocate(B2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2134',-1.000,
     & V2A,U1)
       V2A=V2A+U1
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N1-N1/),'2341',VAHHPP,D1)
       allocate(S46(N0+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S46)
       deallocate(D1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(S46),size(S46),shape(D1),size(D1),
     & (/N0-N0,N1-N1,N0-N0,N0-N0/),'2431',S46,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q11(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q11
       deallocate(Q11)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(S46),size(S46),shape(D1),size(D1),
     & (/N1-N1,N0-N0,N0-N0,N0-N0/),'4231',S46,D1)
       allocate(S47(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S47)
       deallocate(D1)
C
       call sum_stripe(4,shape(X4),size(X4),'3124',1.000,X4,
     & S47)
C
       call sum_stripe(4,shape(X4),size(X4),'3412',1.000,X4,
     & VAHHHH)
C
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N1-N1,N1-N1/),'3412',t2A,D2)
       allocate(U5(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K3
       I3=K1*K1
       call EGEMM(I1,I2,I3,X4,D2,U5)
       deallocate(D2)
C
       V2A=V2A+0.500*U5
       deallocate(U5)
       deallocate(X4)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(S47),size(S47),shape(D1),size(D1),
     & (/N0-N0,N0-N0,N0-N0,N0-N0/),'3214',S47,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S60(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X7),size(X7),'2134',1.000,X7,
     & S60)
       deallocate(S60)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(U9(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,X7,B2,U9)
       deallocate(B2)
C
       V2A=V2A+U9
       deallocate(U9)
       deallocate(X7)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(Q8(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q8)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X2),size(X2),'21',0.500,X2,
     & Q8)
       deallocate(Q8)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,N0-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q12(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q13(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,Q12,B2,Q13)
       deallocate(B2)
C
       call sum_stripe(2,shape(X3),size(X3),'21',-1.000,X3,
     & Q13)
       deallocate(Q13)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(S55(N0+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,S55)
       deallocate(D1)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(S55),size(S55),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S55,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q14(N0+1:N1,N0+1:N1))
       I1=K1*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q14
       deallocate(Q14)
C
       call sum_stripe(2,shape(X2),size(X2),'21',1.000,X2,
     & FAHH)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(t2A),size(t2A),shape(D2),size(D2),
     & (/N0-N0,N1-N1,N1-N1,N0-N0/),'3124',t2A,D2)
       allocate(U3(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K3*K3
       I3=K1
       call EGEMM(I1,I2,I3,X2,D2,U3)
       deallocate(D2)
C
       V2A=V2A+U3
       call sum_stripe(4,shape(V2A),size(V2A),'1243',-1.000,
     & V2A,U3)
       deallocate(U3)
       deallocate(X2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(S55),size(S55),shape(D1),size(D1),
     & (/N0-N0,N2-N2,N0-N0,N0-N0/),'2431',S55,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N2-N2,N1-N1,N0-N0/),'3124',t2B,D2)
       allocate(S56(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S56)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2314',1.000,X8,
     & S56)
       deallocate(S56)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(U11(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,X8,B2,U11)
       deallocate(B2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2134',-1.000,
     & V2A,U11)
       V2A=V2A+U11
       call sum_stripe(4,shape(V2A),size(V2A),'2143',1.000,
     & V2A,U11)
       call sum_stripe(4,shape(V2A),size(V2A),'1243',-1.000,
     & V2A,U11)
       deallocate(U11)
       deallocate(X8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,N0-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q15(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q16(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,Q15,B2,Q16)
       deallocate(B2)
C
       call sum_stripe(2,shape(X3),size(X3),'21',-1.000,X3,
     & Q16)
       deallocate(Q16)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(t2B),size(t2B),shape(D2),size(D2),
     & (/N0-N0,N0-N0,N2-N2,N1-N1/),'3412',t2B,D2)
       allocate(Q9(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q9)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(2,shape(X3),size(X3),'21',-1.000,X3,
     & Q9)
       deallocate(Q9)
C
       X3=X3+FAPP
C
       allocate(U4(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K1*K3
       I3=K3
       call EGEMM(I1,I2,I3,X3,t2A,U4)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2341',1.000,
     & V2A,U4)
       call sum_stripe(4,shape(V2A),size(V2A),'1342',-1.000,
     & V2A,U4)
       deallocate(U4)
       deallocate(X3)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/N1-N1,N1-N1,N1-N1,N0-N0/),'3124',VAHPPP,D1)
       allocate(U2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,t1A,U2)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2A),size(V2A),'3124',1.000,
     & V2A,U2)
       call sum_stripe(4,shape(V2A),size(V2A),'4123',-1.000,
     & V2A,U2)
       deallocate(U2)
C
       end
