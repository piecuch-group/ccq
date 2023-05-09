       subroutine mm_t3d(N0,N1,N2,N3,V3D,
     & K1,K2,K3,K4,
     & FockR,FockB,IntR,IntB,IntM,
     & t1A,t1B,t2A,t2B,t2C)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 FockR(N3,N3)
       real*8 FockB(N3,N3)
       real*8 IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
C
       real*8 V3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
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
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U1(:,:,:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U4(:,:,:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::U10(:,:,:,:,:,:)
       real*8,allocatable::U11(:,:,:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::U12(:,:,:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::U15(:,:,:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::U16(:,:,:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::U29(:,:,:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::U30(:,:,:,:,:,:)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N0-n0,N0-n0,N0-n0,N0-n0/),'3412',IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(S1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X1=0.0d0
       call sum_stripe(4,shape(X1),size(X1),'2134',1.000,X1,
     & S1)
       deallocate(S1)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N2-n0,N0-n0,N2-n0,N0-n0/),'1342',IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S2(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X3=0.0d0
       call sum_stripe(4,shape(X3),size(X3),'3124',1.000,X3,
     & S2)
       deallocate(S2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N2-n0,N0-n0/),'3142',IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(S3(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X4=0.0d0
       call sum_stripe(4,shape(X4),size(X4),'3124',1.000,X4,
     & S3)
       deallocate(S3)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N2-n0,N2-n0/),'1234',IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S4(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       I1=K4*K4*K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X2=0.0d0
       call sum_stripe(4,shape(X2),size(X2),'4123',-1.000,
     & X2,S4)
       deallocate(S4)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N0-n0,N0-n0/),'4231',IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2B),size(t2B),'4213',t2B,D2)
       allocate(S5(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S5)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',-1.000,
     & X3,S5)
       deallocate(S5)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N2-n0,N2-n0/),'4213',IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2B),size(t2B),'4213',t2B,D2)
       allocate(S6(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S6)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X4),size(X4),'3412',1.000,X4,
     & S6)
       deallocate(S6)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_shift(2,shape(FockB),size(FockB),shape(B1),
     & size(B1),(/N2-n0,N0-n0/),'12',FockB,B1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'1234',t2C,D2)
       allocate(S7(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S7)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S7)
       deallocate(S7)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N0-n0,N0-n0,N2-n0,N0-n0/),'3412',IntB,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(t2C),size(t2C),'3412',t2C,D2)
       allocate(S8(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K4
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S8)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X5(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X5=0.0d0
       call sum_stripe(4,shape(X5),size(X5),'2314',1.000,X5,
     & S8)
       deallocate(S8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N0-n0,N0-n0/),'4132',IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(S9(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S9)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(S9),size(S9),'3124',S9,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(U11(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,D2,U11)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'125346',
     & -1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'135246',
     & 1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'234156',
     & 1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'235146',
     & -1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'134256',
     & -1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'124356',
     & 1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & 1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & -1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'234165',
     & -1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'236145',
     & 1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'134265',
     & 1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'124365',
     & -1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'126354',
     & -1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'136254',
     & 1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'235164',
     & 1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'236154',
     & -1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'135264',
     & -1.000,V3D,U11)
       call sum_stripe(6,shape(V3D),size(V3D),'125364',
     & 1.000,V3D,U11)
       deallocate(U11)
       deallocate(S9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N2-n0,N2-n0/),'3214',IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(S10(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S10)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X6(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       X6=0.0d0
       call sum_stripe(4,shape(X6),size(X6),'3412',1.000,X6,
     & S10)
       deallocate(S10)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N0-n0,N2-n0/),'1234',IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'1234',t2C,D2)
       allocate(S11(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K2
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S11)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'3412',0.500,X1,
     & S11)
       deallocate(S11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N0-n0,N2-n0/),'4231',IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
       allocate(Q1(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(Q1),size(Q1),'21',Q1,B1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'1234',t2C,D2)
       allocate(S12(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S12)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S12)
       deallocate(S12)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N2-n0,N0-n0,N0-n0,N0-n0/),'1342',IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S13(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(S13),size(S13),'3214',S13,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(S15(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X8=0.0d0
       call sum_stripe(4,shape(X8),size(X8),'2134',1.000,X8,
     & S15)
       deallocate(S15)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(S13),size(S13),'2314',S13,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(S14(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X7=0.0d0
       call sum_stripe(4,shape(X7),size(X7),'2134',1.000,X7,
     & S14)
       deallocate(S14)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N0-n0,N0-n0,N2-n0,N0-n0/),'4312',IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(S16(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(S16),size(S16),'2314',S16,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(S17(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',1.000,X2,
     & S17)
       deallocate(S17)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N2-n0,N0-n0,N2-n0,N2-n0/),'2314',IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S18(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(S18),size(S18),'2341',S18,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(S20(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'3124',1.000,X4,
     & S20)
       deallocate(S20)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(S18),size(S18),'3241',S18,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S19(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X1),size(X1),'3124',1.000,X1,
     & S19)
       deallocate(S19)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N2-n0,N0-n0,N0-n0,N1-n0/),'1342',IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S21(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(S21),size(S21),'3421',S21,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2B),size(t2B),'4213',t2B,D2)
       allocate(S22(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S22)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',-1.000,
     & X3,S22)
       deallocate(S22)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(U4(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,X3,D2,U4)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234156',
     & 1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'134256',
     & -1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'124356',
     & 1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'235146',
     & -1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'135246',
     & 1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'125346',
     & -1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'234165',
     & -1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'134265',
     & 1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'124365',
     & -1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'235164',
     & 1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'135264',
     & -1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'125364',
     & 1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'236145',
     & 1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & -1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & 1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'236154',
     & -1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'136254',
     & 1.000,V3D,U4)
       call sum_stripe(6,shape(V3D),size(V3D),'126354',
     & -1.000,V3D,U4)
       deallocate(U4)
       deallocate(X3)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N0-n0,N2-n0/),'4231',IntM,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2B),size(t2B),'4213',t2B,D2)
       allocate(S23(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S23)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(S23),size(S23),'3412',S23,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(S24(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X4),size(X4),'2134',-1.000,
     & X4,S24)
       deallocate(S24)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'1234',t2C,D2)
       allocate(U5(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,X4,D2,U5)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345126',
     & -1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'245136',
     & 1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'345216',
     & 1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'245316',
     & -1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'145236',
     & -1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'145326',
     & 1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'346125',
     & 1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'246135',
     & -1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'346215',
     & -1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'246315',
     & 1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'146235',
     & 1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'146325',
     & -1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'356124',
     & -1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'256134',
     & 1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'356214',
     & 1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'256314',
     & -1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'156234',
     & -1.000,V3D,U5)
       call sum_stripe(6,shape(V3D),size(V3D),'156324',
     & 1.000,V3D,U5)
       deallocate(U5)
       deallocate(X4)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N2-n0,N0-n0,N0-n0,N2-n0/),'1342',IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S25(N0+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(S25),size(S25),'3421',S25,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(S27(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S27)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X8),size(X8),'2314',1.000,X8,
     & S27)
       deallocate(S27)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(U16(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,X8,D2,U16)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124356',
     & -1.000,V3D,U16)
       call sum_stripe(6,shape(V3D),size(V3D),'125346',
     & 1.000,V3D,U16)
       call sum_stripe(6,shape(V3D),size(V3D),'124365',
     & 1.000,V3D,U16)
       call sum_stripe(6,shape(V3D),size(V3D),'125364',
     & -1.000,V3D,U16)
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & -1.000,V3D,U16)
       call sum_stripe(6,shape(V3D),size(V3D),'126354',
     & 1.000,V3D,U16)
       deallocate(U16)
       deallocate(X8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(S25),size(S25),'2431',S25,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(S28(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S28)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X7),size(X7),'2314',1.000,X7,
     & S28)
       deallocate(S28)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(U15(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,X7,D2,U15)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234156',
     & 1.000,V3D,U15)
       call sum_stripe(6,shape(V3D),size(V3D),'134256',
     & -1.000,V3D,U15)
       call sum_stripe(6,shape(V3D),size(V3D),'235146',
     & -1.000,V3D,U15)
       call sum_stripe(6,shape(V3D),size(V3D),'135246',
     & 1.000,V3D,U15)
       call sum_stripe(6,shape(V3D),size(V3D),'234165',
     & -1.000,V3D,U15)
       call sum_stripe(6,shape(V3D),size(V3D),'134265',
     & 1.000,V3D,U15)
       call sum_stripe(6,shape(V3D),size(V3D),'235164',
     & 1.000,V3D,U15)
       call sum_stripe(6,shape(V3D),size(V3D),'135264',
     & -1.000,V3D,U15)
       call sum_stripe(6,shape(V3D),size(V3D),'236145',
     & 1.000,V3D,U15)
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & -1.000,V3D,U15)
       call sum_stripe(6,shape(V3D),size(V3D),'236154',
     & -1.000,V3D,U15)
       call sum_stripe(6,shape(V3D),size(V3D),'136254',
     & 1.000,V3D,U15)
       deallocate(U15)
       deallocate(X7)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(S25),size(S25),'4231',S25,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S35(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(S35),size(S35),'2314',S35,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(S36(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X9=0.0d0
       call sum_stripe(4,shape(X9),size(X9),'2134',1.000,X9,
     & S36)
       deallocate(S36)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(S25),size(S25),'3241',S25,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(S38(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(S38),size(S38),'2314',S38,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(S39(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S39)
       deallocate(S39)
C
       call sumx1423(N0,N3,N2,N3,N2,N3,N2,N3,N0,N2,X2,IntB, 1.000)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'1234',t2C,D2)
       allocate(U2(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,X2,D2,U2)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'345126',
     & -1.000,V3D,U2)
       call sum_stripe(6,shape(V3D),size(V3D),'245136',
     & 1.000,V3D,U2)
       call sum_stripe(6,shape(V3D),size(V3D),'145236',
     & -1.000,V3D,U2)
       call sum_stripe(6,shape(V3D),size(V3D),'346125',
     & 1.000,V3D,U2)
       call sum_stripe(6,shape(V3D),size(V3D),'246135',
     & -1.000,V3D,U2)
       call sum_stripe(6,shape(V3D),size(V3D),'146235',
     & 1.000,V3D,U2)
       call sum_stripe(6,shape(V3D),size(V3D),'356124',
     & -1.000,V3D,U2)
       call sum_stripe(6,shape(V3D),size(V3D),'256134',
     & 1.000,V3D,U2)
       call sum_stripe(6,shape(V3D),size(V3D),'156234',
     & -1.000,V3D,U2)
       deallocate(U2)
       deallocate(X2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(S25),size(S25),'2341',S25,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(t2C),size(t2C),'3412',t2C,D2)
       allocate(S26(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K4*K4
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S26)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X5),size(X5),'2314',-1.000,
     & X5,S26)
       deallocate(S26)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'1234',t2C,D2)
       allocate(U10(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,X5,D2,U10)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'145236',
     & -0.500,V3D,U10)
       call sum_stripe(6,shape(V3D),size(V3D),'245136',
     & 0.500,V3D,U10)
       call sum_stripe(6,shape(V3D),size(V3D),'345126',
     & -0.500,V3D,U10)
       call sum_stripe(6,shape(V3D),size(V3D),'146235',
     & 0.500,V3D,U10)
       call sum_stripe(6,shape(V3D),size(V3D),'246135',
     & -0.500,V3D,U10)
       call sum_stripe(6,shape(V3D),size(V3D),'346125',
     & 0.500,V3D,U10)
       call sum_stripe(6,shape(V3D),size(V3D),'156234',
     & -0.500,V3D,U10)
       call sum_stripe(6,shape(V3D),size(V3D),'256134',
     & 0.500,V3D,U10)
       call sum_stripe(6,shape(V3D),size(V3D),'356124',
     & -0.500,V3D,U10)
       deallocate(U10)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(S35),size(S35),'3214',S35,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(S37(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X10=0.0d0
       call sum_stripe(4,shape(X10),size(X10),'2134',1.000,
     & X10,S37)
       deallocate(S37)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N0-n0,N0-n0/),'1234',IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'1234',t2C,D2)
       allocate(S31(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S31)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(S31),size(S31),'3412',S31,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(S32(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X9),size(X9),'2134',-0.500,
     & X9,S32)
       deallocate(S32)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(U29(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,X9,D2,U29)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234156',
     & -1.000,V3D,U29)
       call sum_stripe(6,shape(V3D),size(V3D),'134256',
     & 1.000,V3D,U29)
       call sum_stripe(6,shape(V3D),size(V3D),'235146',
     & 1.000,V3D,U29)
       call sum_stripe(6,shape(V3D),size(V3D),'135246',
     & -1.000,V3D,U29)
       call sum_stripe(6,shape(V3D),size(V3D),'236145',
     & -1.000,V3D,U29)
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & 1.000,V3D,U29)
       deallocate(U29)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(S31),size(S31),'4312',S31,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(S34(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X10),size(X10),'2134',-0.500,
     & X10,S34)
       deallocate(S34)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(U30(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,X10,D2,U30)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'124356',
     & 1.000,V3D,U30)
       call sum_stripe(6,shape(V3D),size(V3D),'125346',
     & -1.000,V3D,U30)
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & 1.000,V3D,U30)
       deallocate(U30)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N0-n0,N2-n0/),'4231',IntB,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(S29(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S29)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(S29),size(S29),'3412',S29,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(S30(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X6),size(X6),'2134',1.000,X6,
     & S30)
       deallocate(S30)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'1234',t2C,D2)
       allocate(U12(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,X6,D2,U12)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'256134',
     & -1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'356124',
     & 1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'156234',
     & 1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'156324',
     & -1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'356214',
     & -1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'256314',
     & 1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'246135',
     & 1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'346125',
     & -1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'146235',
     & -1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'146325',
     & 1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'346215',
     & 1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'246315',
     & -1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'345126',
     & 1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'245136',
     & -1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'345216',
     & -1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'245316',
     & 1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'145236',
     & 1.000,V3D,U12)
       call sum_stripe(6,shape(V3D),size(V3D),'145326',
     & -1.000,V3D,U12)
       deallocate(U12)
       deallocate(X6)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N0-n0,N2-n0/),'3142',IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(Q2(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(Q2),size(Q2),'21',Q2,B1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'1234',t2C,D2)
       allocate(S33(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S33)
       deallocate(B1)
       deallocate(D2)
       deallocate(Q2)
C
       call sum_stripe(4,shape(X1),size(X1),'2341',1.000,X1,
     & S33)
       deallocate(S33)
C
       call sumx3412(N0,N3,N0,N2,N2,N3,N0,N2,N0,N2,X1,IntB, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(U1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,X1,D2,U1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3D),size(V3D),'234156',
     & 1.000,V3D,U1)
       call sum_stripe(6,shape(V3D),size(V3D),'134256',
     & -1.000,V3D,U1)
       call sum_stripe(6,shape(V3D),size(V3D),'124356',
     & 1.000,V3D,U1)
       call sum_stripe(6,shape(V3D),size(V3D),'235146',
     & -1.000,V3D,U1)
       call sum_stripe(6,shape(V3D),size(V3D),'135246',
     & 1.000,V3D,U1)
       call sum_stripe(6,shape(V3D),size(V3D),'125346',
     & -1.000,V3D,U1)
       call sum_stripe(6,shape(V3D),size(V3D),'236145',
     & 1.000,V3D,U1)
       call sum_stripe(6,shape(V3D),size(V3D),'136245',
     & -1.000,V3D,U1)
       call sum_stripe(6,shape(V3D),size(V3D),'126345',
     & 1.000,V3D,U1)
       deallocate(U1)
       deallocate(X1)
C
       end
