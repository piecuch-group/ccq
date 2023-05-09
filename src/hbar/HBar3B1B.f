       subroutine HBar3B1B(N0,N1,N2,N3,V3B1B,
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
       real*8 V3B1B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::S3(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U4(:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:)
       real*8,allocatable::U8(:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U9(:,:,:,:)
       real*8,allocatable::U10(:,:,:,:)
       real*8,allocatable::U11(:,:,:,:)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N0-n0,N1-n0/),'3142',IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2B),size(t2B),'4213',t2B,D2)
       allocate(S3(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S3)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       X2=0.0d0
       call sum_stripe(4,shape(X2),size(X2),'2413',1.000,X2,
     & S3)
       deallocate(S3)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N0-n0,N1-n0/),'3142',IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
       allocate(Q1(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,N1+1:N3))
       X1=0.0d0
       X1=X1+Q1
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N0-n0,N1-n0,N0-n0/),'4321',IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
       allocate(S12(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       X3=0.0d0
       call sum_stripe(4,shape(X3),size(X3),'3124',-1.000,
     & X3,S12)
       deallocate(S12)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N2-n0,N0-n0,N2-n0,N1-n0/),'1432',IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S14(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X2),size(X2),'4123',1.000,X2,
     & S14)
       deallocate(S14)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N0-n0,N1-n0/),'3142',IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(S16(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S16)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2413',1.000,X2,
     & S16)
       deallocate(S16)
C
       call sumx4321(N0,N3,N0,N1,N2,N3,N1,N3,N0,N2,X2,IntM, 1.000)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
       allocate(U4(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,X2,B2,U4)
       deallocate(B2)
C
       call sum_stripe(4,shape(V3B1B),size(V3B1B),'3124',
     & -1.000,V3B1B,U4)
       deallocate(U4)
       deallocate(X2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N2-n0,N0-n0,N1-n0,N1-n0/),'1324',IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S18(N0+1:N2,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'4123',1.000,X3,
     & S18)
       deallocate(S18)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N2-n0,N0-n0,N0-n0,N1-n0/),'1342',IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S20(N0+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       X4=0.0d0
       call sum_stripe(4,shape(X4),size(X4),'4123',1.000,X4,
     & S20)
C
       call sumx4312(N0,N3,N0,N2,N0,N1,N1,N3,N0,N2,X4,IntM, 1.000)
C
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(t2B),size(t2B),'3412',t2B,D2)
       allocate(U9(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K3*K4
       I3=K1*K2
       call EGEMM(I1,I2,I3,X4,D2,U9)
       deallocate(D2)
C
       call sum_stripe(4,shape(V3B1B),size(V3B1B),'1324',
     & 1.000,V3B1B,U9)
       deallocate(U9)
       deallocate(X4)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(S20),size(S20),'3241',S20,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
       allocate(S25(N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N2))
       I1=K2*K3*K2
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(X3),size(X3),'3124',-1.000,
     & X3,S25)
       deallocate(S25)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N0-n0,N1-n0/),'3142',IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(Q2(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q2
       deallocate(Q2)
C
       call sumx21(0,N3,N0,N1,N1,N3,X1,FockR, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2B),size(t2B),'4123',t2B,D2)
       allocate(U1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3))
       I1=K3
       I2=K2*K3*K4
       I3=K1
       call EGEMM(I1,I2,I3,X1,D2,U1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V3B1B),size(V3B1B),'1342',
     & -1.000,V3B1B,U1)
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N0-n0,N1-n0/),'4132',IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2B),size(t2B),'4123',t2B,D2)
       allocate(S22(N1+1:N3,N0+1:N2,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K2*K3
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S22)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'3412',-1.000,
     & X3,S22)
       deallocate(S22)
C
       call sumx4213(N0,N3,N0,N2,N1,N3,N1,N3,N0,N2,X3,IntM, 1.000)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(U7(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K3
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,X3,B2,U7)
       deallocate(B2)
C
       V3B1B=V3B1B-U7
       deallocate(U7)
       deallocate(X3)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N1-n0,N1-n0/),'3124',IntR,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2B),size(t2B),'4213',t2B,D2)
       allocate(U2(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,U2)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V3B1B),size(V3B1B),'1423',
     & 1.000,V3B1B,U2)
       deallocate(U2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N1-n0,N1-n0/),'1324',IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(U8(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       I1=K3*K3*K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,U8)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(V3B1B),size(V3B1B),'4123',
     & 1.000,V3B1B,U8)
       deallocate(U8)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N2-n0,N1-n0/),'4132',IntM,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2B),size(t2B),'4123',t2B,D2)
       allocate(U10(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K2*K3
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,U10)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V3B1B),size(V3B1B),'3412',
     & -1.000,V3B1B,U10)
       deallocate(U10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N1-n0,N1-n0/),'3124',IntM,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(U11(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,U11)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V3B1B),size(V3B1B),'1423',
     & 1.000,V3B1B,U11)
       deallocate(U11)
C
       end
