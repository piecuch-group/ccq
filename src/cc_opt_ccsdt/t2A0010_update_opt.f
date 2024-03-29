       subroutine t2A0010_update(N0,N1,N2,N3,HT2A,shift,
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
       real*8 HT2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
C
       real*8,allocatable::V2A(:,:,:,:)
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F1(:,:,:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::S57(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::Z1(:,:,:,:)
       real*8,allocatable::X2(:,:)
       real*8,allocatable::Z2(:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::Z3(:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::Z4(:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::Z5(:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::Z6(:,:,:,:)
       real*8,allocatable::Z7(:,:,:,:)
       real*8,allocatable::Z8(:,:,:,:)
       real*8,allocatable::Z9(:,:,:,:)
       real*8,allocatable::Z10(:,:,:,:)
       real*8,allocatable::Z11(:,:,:,:)
       real*8,allocatable::X7(:,:)
       real*8,allocatable::Z12(:,:,:,:)
       real*8,allocatable::X8(:,:)
       real*8,allocatable::Z13(:,:,:,:)
       real*8,allocatable::Z14(:,:,:,:)
       real*8,allocatable::Z15(:,:,:,:)
       real*8,allocatable::Z16(:,:,:,:)
       real*8,allocatable::Z17(:,:,:,:)
       real*8,allocatable::Z18(:,:,:,:)
       real*8,allocatable::Z19(:,:,:,:)
       real*8,allocatable::Z20(:,:,:,:)
       real*8,allocatable::Z21(:,:,:,:)
       real*8,allocatable::Z22(:,:,:,:)
       real*8,allocatable::Z23(:,:,:,:)
       real*8,allocatable::Z24(:,:,:,:)
       real*8,allocatable::Z25(:,:,:,:)
       real*8,allocatable::Z26(:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::Z36(:,:,:,:)
       real*8,allocatable::Z42(:,:,:,:)
       real*8,allocatable::Z44(:,:,:,:)
       real*8,allocatable::Z48(:,:,:,:)
       real*8,allocatable::Z50(:,:,:,:)
       real*8,allocatable::Z52(:,:,:,:)
       real*8,allocatable::Z54(:,:,:,:)
       real*8,allocatable::Z56(:,:,:,:)
       real*8,allocatable::Z58(:,:,:,:)
       real*8,allocatable::Z68(:,:,:,:)
       real*8,allocatable::Z70(:,:,:,:)
       real*8,allocatable::Z72(:,:,:,:)
       real*8,allocatable::Z73(:,:,:,:)
       real*8,allocatable::Z74(:,:,:,:)
C
       allocate(V2A(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       V2A=0.0d0
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,M2-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S27(N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       X3=0.0d0
       call sum_stripe(4,shape(X3),size(X3),'4123',-1.000,
     & X3,S27)
       deallocate(S27)
C
       call sumx_sorted3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N0,M1,X3,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N1,M2-N1,N1-N1,N0-N0/),'461235',t3A,F2)
       allocate(Z3(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,X3,F2,Z3)
       deallocate(F2)
C
       V2A=V2A-Z3
       call sum_stripe(4,shape(V2A),size(V2A),'1243',1.000,
     & V2A,Z3)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,M2-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S29(N0+1:M1,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       X4=0.0d0
       call sum_stripe(4,shape(X4),size(X4),'4123',-1.000,
     & X4,S29)
       deallocate(S29)
C
       call sumx_sorted3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N0,M1,X4,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N1,M2-N1,N1-N1,N0-N0/),'561234',t3A,F2)
       allocate(Z4(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,X4,F2,Z4)
       deallocate(F2)
C
       V2A=V2A+0.500*Z4
       call sum_stripe(4,shape(V2A),size(V2A),'1243',-0.500,
     & V2A,Z4)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S31(N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       X5=0.0d0
       call sum_stripe(4,shape(X5),size(X5),'4123',-1.000,
     & X5,S31)
       deallocate(S31)
C
       call sumx_sorted3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N0,M1,X5,VAHHHP, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N1-N1,M2-N1,N1-N1,N0-N0/),'462135',t3A,F2)
       allocate(Z5(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,X5,F2,Z5)
       deallocate(F2)
C
       V2A=V2A+Z5
       call sum_stripe(4,shape(V2A),size(V2A),'1243',-1.000,
     & V2A,Z5)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,N1-N1/),'1342',VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S33(N0+1:M1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       X6=0.0d0
       call sum_stripe(4,shape(X6),size(X6),'4123',-1.000,
     & X6,S33)
       deallocate(S33)
C
       call sumx_sorted3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N0,M1,X6,VAHHHP, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N1-N1,M2-N1,N1-N1,N0-N0/),'562134',t3A,F2)
       allocate(Z6(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,X6,F2,Z6)
       deallocate(F2)
C
       V2A=V2A-0.500*Z6
       call sum_stripe(4,shape(V2A),size(V2A),'1243',0.500,
     & V2A,Z6)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,M2-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(S35(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S35)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X9(N0+1:N1,N1+1:M2,N0+1:M1,N0+1:M1))
       X9=0.0d0
       call sum_stripe(4,shape(X9),size(X9),'2341',1.000,X9,
     & S35)
       deallocate(S35)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N1-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(S37(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S37)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',2.000,X9,
     & S37)
       deallocate(S37)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N0-N0/),'4123',VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N1-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(S39(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S39)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',1.000,X9,
     & S39)
       deallocate(S39)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N1,N1-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S41(N1+1:M2,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S41),size(S41),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,N1-N1/),'2341',S41,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N1-N1,M2-N1,N0-N0,N0-N0/),'613245',t3A,F2)
       allocate(Z42(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z42)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'1342',-1.000,
     & V2A,Z42)
       deallocate(Z42)
       deallocate(S41)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N1-N1,N1-N1/),'4312',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S43(N1+1:M2,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S43),size(S43),shape(D1),size(D1),
     & (/M1-M1,N1-N1,N1-N1,N1-N1/),'2341',S43,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N1-N1,M2-N1,N0-N0,N0-N0/),'623145',t3A,F2)
       allocate(Z44(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z44)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'1342',0.500,
     & V2A,Z44)
       deallocate(Z44)
       deallocate(S43)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q1(M1+1:N1,M2+1:N3))
       I1=K6*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(M1+1:N1,M2+1:N3))
       X1=0.0d0
       X1=X1+Q1
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VAHHPP),size(VAHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N1-N1/),'3142',VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q2(M1+1:N1,N1+1:M2))
       I1=K9*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(M1+1:N1,N1+1:M2))
       X2=0.0d0
       X2=X2+Q2
       deallocate(Q2)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S47(N0+1:M1,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S47),size(S47),shape(D1),size(D1),
     & (/M1-M1,N0-N0,M2-M2,N0-N0/),'2341',S47,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-N2,M2-N1,N1-N1,N0-N0/),'451236',t3B4,F2)
       allocate(Z48(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z48)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z48
       call sum_stripe(4,shape(V2A),size(V2A),'1243',-1.000,
     & V2A,Z48)
       deallocate(Z48)
       deallocate(S47)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S49(N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S49),size(S49),shape(D1),size(D1),
     & (/N0-N0,M1-M1,M2-M2,N0-N0/),'2341',S49,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(Z50(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z50)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z50
       call sum_stripe(4,shape(V2A),size(V2A),'1243',1.000,
     & V2A,Z50)
       deallocate(Z50)
       deallocate(S49)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,M2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S51(N0+1:M1,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(S51),size(S51),shape(D1),size(D1),
     & (/M1-M1,M1-M1,M2-M2,N0-N0/),'2341',S51,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(Z52(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z52)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z52
       call sum_stripe(4,shape(V2A),size(V2A),'1243',1.000,
     & V2A,Z52)
       deallocate(Z52)
       deallocate(S51)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,N0-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S53(N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S53),size(S53),shape(D1),size(D1),
     & (/M1-M1,N0-N0,N2-N2,N0-N0/),'2341',S53,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N2-N2,M2-N1,N1-N1,N0-N0/),'451236',t3B4,F2)
       allocate(Z54(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z54)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z54
       call sum_stripe(4,shape(V2A),size(V2A),'1243',-1.000,
     & V2A,Z54)
       deallocate(Z54)
       deallocate(S53)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,N0-N0,M1-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S55(N0+1:M1,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S55),size(S55),shape(D1),size(D1),
     & (/N0-N0,M1-M1,N2-N2,N0-N0/),'2341',S55,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(Z56(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z56)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z56
       call sum_stripe(4,shape(V2A),size(V2A),'1243',1.000,
     & V2A,Z56)
       deallocate(Z56)
       deallocate(S55)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N1-N1,M1-N0,M1-N0,N2-N2/),'2341',VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N1-N1,N0-N0/),'12',t1A,B2)
       allocate(S57(N0+1:M1,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(S57),size(S57),shape(D1),size(D1),
     & (/M1-M1,M1-M1,N2-N2,N0-N0/),'2341',S57,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(Z58(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z58)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z58
       call sum_stripe(4,shape(V2A),size(V2A),'1243',1.000,
     & V2A,Z58)
       deallocate(Z58)
       deallocate(S57)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(S59(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S59)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-2.000,
     & X9,S59)
       deallocate(S59)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(S61(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S61)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-2.000,
     & X9,S61)
       deallocate(S61)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(S63(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S63)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-2.000,
     & X9,S63)
       deallocate(S63)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N0-N0/),'3124',VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(S65(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S65)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(X9),size(X9),'2341',-2.000,
     & X9,S65)
       deallocate(S65)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,M2-N1/),'21',t1A,B2)
       allocate(Z36(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,X9,B2,Z36)
       deallocate(B2)
C
       V2A=V2A+0.500*Z36
       deallocate(Z36)
       deallocate(X9)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,M2-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S67(N1+1:M2,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(S67),size(S67),shape(D1),size(D1),
     & (/M1-M1,N2-N2,M2-M2,N1-N1/),'2341',S67,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B2),size(t3B2),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-M2,M2-M2,N0-N0,N0-N0/),'412356',t3B2,F2)
       allocate(Z68(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z68)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'1342',1.000,
     & V2A,Z68)
       deallocate(Z68)
       deallocate(S67)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N1-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S69(N1+1:M2,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S69),size(S69),shape(D1),size(D1),
     & (/M1-M1,M2-M2,N1-N1,N1-N1/),'2341',S69,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,M2-N1,N0-N0,N0-N0/),'413256',t3B4,F2)
       allocate(Z70(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z70)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'1342',-1.000,
     & V2A,Z70)
       deallocate(Z70)
       deallocate(S69)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N1-N1/),'4312',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(S71(N1+1:M2,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(S71),size(S71),shape(D1),size(D1),
     & (/M1-M1,N2-N2,N1-N1,N1-N1/),'2341',S71,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,M2-N1,N0-N0,N0-N0/),'413256',t3B4,F2)
       allocate(Z72(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z72)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'1342',-1.000,
     & V2A,Z72)
       deallocate(Z72)
       deallocate(S71)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,M2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q3(M1+1:N2,M2+1:N3))
       I1=K6*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z73(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM2(I2,I3,Q3,F2,Z73)
       deallocate(F2)
C
       V2A=V2A+Z73
       deallocate(Z73)
       deallocate(Q3)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N1-N1,M1-N0,N2-N2/),'4231',VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_shift(2,shape(t1A),size(t1A),shape(B2),size(B2),
     & (/N0-N0,N1-N1/),'21',t1A,B2)
       allocate(Q4(M1+1:N2,N2+1:M2))
       I1=K0*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z74(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM2(I2,I3,Q4,F2,Z74)
       deallocate(F2)
C
       V2A=V2A+Z74
       deallocate(Z74)
       deallocate(Q4)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q5(M1+1:N1,M2+1:N3))
       I1=K6*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q5
       deallocate(Q5)
C
       call sumx_sorted21(N1,N3,N0,N1,
     & M1,N1,M2,N3,X1,FAHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(Z1(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K9*K6
       I3=K6*K7
       call EGEMM2(I2,I3,X1,F2,Z1)
       deallocate(F2)
C
       V2A=V2A+Z1
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder_shift(4,shape(VBHHPP),size(VBHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N1-N1/),'3142',VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q6(M1+1:N1,N1+1:M2))
       I1=K9*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       X2=X2+Q6
       deallocate(Q6)
C
       call sumx_sorted21(N1,N3,N0,N1,
     & M1,N1,N1,M2,X2,FAHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,N1-N1,M2-N1,N1-N1,N0-N0,N0-N0/),'621345',t3A,F2)
       allocate(Z2(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K9*K6
       I3=K9*K7
       call EGEMM2(I2,I3,X2,F2,Z2)
       deallocate(F2)
C
       V2A=V2A-Z2
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,M2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q7(M1+1:N2,M2+1:N3))
       I1=K6*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(M1+1:N2,M2+1:N3))
       X7=0.0d0
       X7=X7+Q7
       deallocate(Q7)
C
       call sumx_sorted21(N2,N3,N0,N2,
     & M1,N2,M2,N3,X7,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z12(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM2(I2,I3,X7,F2,Z12)
       deallocate(F2)
C
       V2A=V2A+Z12
       deallocate(Z12)
       deallocate(X7)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder_shift(4,shape(VCHHPP),size(VCHHPP),shape(D1),
     & size(D1),(/N0-N0,N2-N2,M1-N0,N2-N2/),'3142',VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_shift(2,shape(t1B),size(t1B),shape(B2),size(B2),
     & (/N0-N0,N2-N2/),'21',t1B,B2)
       allocate(Q8(M1+1:N2,N2+1:M2))
       I1=K0*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(M1+1:N2,N2+1:M2))
       X8=0.0d0
       X8=X8+Q8
       deallocate(Q8)
C
       call sumx_sorted21(N2,N3,N0,N2,
     & M1,N2,N2,M2,X8,FBHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z13(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM2(I2,I3,X8,F2,Z13)
       deallocate(F2)
C
       V2A=V2A+Z13
       deallocate(Z13)
       deallocate(X8)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,M2-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,M2-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(Z7(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z7)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2341',-0.500,
     & V2A,Z7)
       deallocate(Z7)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N1-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(Z8(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z8)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2341',-1.000,
     & V2A,Z8)
       deallocate(Z8)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,M2-N1/),'4123',VAHPPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N1-N1,N1-N1,N0-N0,N0-N0/),'612345',t3A,F2)
       allocate(Z9(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z9)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2341',-0.500,
     & V2A,Z9)
       deallocate(Z9)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/M1-N0,M2-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,M2-N1,N1-N1,M2-N1,N0-N0,N0-N0/),'613245',t3A,F2)
       allocate(Z10(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z10)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'1342',-1.000,
     & V2A,Z10)
       deallocate(Z10)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VAHPPP),size(VAHPPP),shape(D1),
     & size(D1),(/M1-N0,N1-N1,N1-N1,N1-N1/),'4123',VAHPPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3A),size(t3A),shape(F2),size(F2),
     & (/M1-M1,N1-N1,N1-N1,M2-N1,N0-N0,N0-N0/),'623145',t3A,F2)
       allocate(Z11(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z11)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'1342',0.500,
     & V2A,Z11)
       deallocate(Z11)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,M2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N0-N0,M2-N2,M2-N1,N1-N1,N0-N0/),'451236',t3B4,F2)
       allocate(Z14(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z14)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z14
       call sum_stripe(4,shape(V2A),size(V2A),'1243',-1.000,
     & V2A,Z14)
       deallocate(Z14)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,M2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,M2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(Z15(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z15)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z15
       call sum_stripe(4,shape(V2A),size(V2A),'1243',1.000,
     & V2A,Z15)
       deallocate(Z15)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,M2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,M2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(Z16(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z16)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z16
       call sum_stripe(4,shape(V2A),size(V2A),'1243',1.000,
     & V2A,Z16)
       deallocate(Z16)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/M1-N0,N0-N0,N2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N0-N0,N2-N2,M2-N1,N1-N1,N0-N0/),'451236',t3B4,F2)
       allocate(Z17(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,Z17)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A+Z17
       call sum_stripe(4,shape(V2A),size(V2A),'1243',-1.000,
     & V2A,Z17)
       deallocate(Z17)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/N0-N0,M1-N0,N2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/N0-N0,M1-M1,N2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(Z18(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z18)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z18
       call sum_stripe(4,shape(V2A),size(V2A),'1243',1.000,
     & V2A,Z18)
       deallocate(Z18)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder_shift(4,shape(VBHHHP),size(VBHHHP),shape(D1),
     & size(D1),(/M1-N0,M1-N0,N2-N2,N0-N0/),'3412',VBHHHP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder_shift(6,shape(t3B1),size(t3B1),shape(F2),size(F2),
     & (/M1-N0,M1-M1,N2-N2,M2-N1,N1-N1,N0-N0/),'461235',t3B1,F2)
       allocate(Z19(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,Z19)
       deallocate(D1)
       deallocate(F2)
C
       V2A=V2A-Z19
       call sum_stripe(4,shape(V2A),size(V2A),'1243',1.000,
     & V2A,Z19)
       deallocate(Z19)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,M2-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z20(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z20)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2341',1.000,
     & V2A,Z20)
       deallocate(Z20)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z21(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z21)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2341',1.000,
     & V2A,Z21)
       deallocate(Z21)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z22(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z22)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2341',1.000,
     & V2A,Z22)
       deallocate(Z22)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,M2-N1/),'3124',VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,N1-N1,N0-N0,N0-N0/),'412356',t3B4,F2)
       allocate(Z23(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K5*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z23)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2341',1.000,
     & V2A,Z23)
       deallocate(Z23)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,M2-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B2),size(t3B2),shape(F2),size(F2),
     & (/M1-M1,N2-N2,M2-M2,M2-M2,N0-N0,N0-N0/),'412356',t3B2,F2)
       allocate(Z24(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z24)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'1342',-1.000,
     & V2A,Z24)
       deallocate(Z24)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/M1-N0,M2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,M2-N2,N1-N1,M2-N1,N0-N0,N0-N0/),'413256',t3B4,F2)
       allocate(Z25(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z25)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'1342',1.000,
     & V2A,Z25)
       deallocate(Z25)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder_shift(4,shape(VBPHPP),size(VBPHPP),shape(D1),
     & size(D1),(/M1-N0,N2-N2,N1-N1,N1-N1/),'3124',VBPHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder_shift(6,shape(t3B4),size(t3B4),shape(F2),size(F2),
     & (/M1-M1,N2-N2,N1-N1,M2-N1,N0-N0,N0-N0/),'413256',t3B4,F2)
       allocate(Z26(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K5*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z26)
       deallocate(D1)
       deallocate(F2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'1342',1.000,
     & V2A,Z26)
       deallocate(Z26)
C
       call sumx_sorted2(N1,N3,N1,N3,N0,N1,N0,N1,
     & M2,N3,N1,M2,N0,M1,N0,M1,HT2A,V2A,1.0)
       deallocate(V2A)
C
       end
