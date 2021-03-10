       subroutine HBar1A3A(N0,N1,N2,N3,V1A3A,
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
       real*8 V1A3A(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::S6(:,:,:,:)
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:)
       real*8,allocatable::U3(:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U4(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:)
       real*8,allocatable::U9(:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U17(:,:,:,:)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N1-n0,N0-n0,N0-n0,N0-n0/),'1423',IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder_stripe(2,shape(t1A),size(t1A),'12',t1A,B2)
       allocate(S6(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder_stripe(4,shape(S6),size(S6),'2134',S6,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
       allocate(U7(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,U7)
       deallocate(D1)
       deallocate(B2)
C
       V1A3A=V1A3A-U7
       call sum_stripe(4,shape(V1A3A),size(V1A3A),'1324',
     & 1.000,V1A3A,U7)
       deallocate(U7)
       deallocate(S6)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N1-n0,N1-n0,N1-n0,N0-n0/),'2143',IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder_stripe(2,shape(t1A),size(t1A),'12',t1A,B2)
       allocate(S8(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_stripe(4,shape(S8),size(S8),'2314',S8,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder_stripe(2,shape(t1A),size(t1A),'12',t1A,B2)
       allocate(U9(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,U9)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(V1A3A),size(V1A3A),'2134',
     & 1.000,V1A3A,U9)
       deallocate(U9)
       deallocate(S8)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N1-n0,N0-n0,N1-n0,N0-n0/),'1423',IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder_stripe(2,shape(t1A),size(t1A),'12',t1A,B2)
       allocate(S10(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3*K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       X3=0.0d0
       call sum_stripe(4,shape(X3),size(X3),'3124',-1.000,
     & X3,S10)
C
       call sumx2341(N0,N3,N0,N1,N1,N3,N0,N1,N0,N1,X3,IntR, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(t2A),size(t2A),'3124',t2A,D2)
       allocate(U4(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K3*K1
       call EGEMM(I1,I2,I3,X3,D2,U4)
       deallocate(D2)
C
       V1A3A=V1A3A+U4
       call sum_stripe(4,shape(V1A3A),size(V1A3A),'1324',
     & -1.000,V1A3A,U4)
       deallocate(U4)
       deallocate(X3)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder_stripe(4,shape(S10),size(S10),'3214',S10,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder_stripe(2,shape(t1A),size(t1A),'12',t1A,B2)
       allocate(S15(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       X2=0.0d0
       call sum_stripe(4,shape(X2),size(X2),'2134',-1.000,
     & X2,S15)
       deallocate(S15)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N1-n0,N0-n0/),'3124',IntR,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
       allocate(Q1(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N1+1:N3,N0+1:N1))
       X1=0.0d0
       X1=X1+Q1
       deallocate(Q1)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N1-n0,N1-n0,N0-n0,N0-n0/),'1243',IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_stripe(4,shape(t2A),size(t2A),'1234',t2A,D2)
       allocate(S12(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K1
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S12)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X2),size(X2),'2314',0.500,X2,
     & S12)
       deallocate(S12)
C
       call sumx2341(N0,N3,N0,N1,N0,N1,N0,N1,N0,N1,X2,IntR, 1.000)
C
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
       allocate(U2(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1*K1
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,X2,B2,U2)
       deallocate(B2)
C
       V1A3A=V1A3A-U2
       deallocate(U2)
       deallocate(X2)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N1-n0,N0-n0,N2-n0,N0-n0/),'2314',IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder_stripe(2,shape(t1A),size(t1A),'12',t1A,B2)
       allocate(S18(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4*K2
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:N1))
       X4=0.0d0
       call sum_stripe(4,shape(X4),size(X4),'3124',1.000,X4,
     & S18)
       deallocate(S18)
C
       call sumx2314(N0,N3,N0,N2,N2,N3,N0,N1,N0,N1,X4,IntM, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(t2B),size(t2B),'3124',t2B,D2)
       allocate(U17(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K1*K3
       I3=K4*K2
       call EGEMM(I1,I2,I3,X4,D2,U17)
       deallocate(D2)
C
       V1A3A=V1A3A-U17
       call sum_stripe(4,shape(V1A3A),size(V1A3A),'1324',
     & 1.000,V1A3A,U17)
       deallocate(U17)
       deallocate(X4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N1-n0,N0-n0/),'3124',IntM,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(Q2(N1+1:N3,N0+1:N1))
       I1=K1*K3
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q2
       deallocate(Q2)
C
       call sumx12(0,N3,N1,N3,N0,N1,X1,FockR, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_stripe(4,shape(t2A),size(t2A),'1234',t2A,D2)
       allocate(U1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K1*K3
       I3=K3
       call EGEMM(I1,I2,I3,X1,D2,U1)
       deallocate(D2)
C
       V1A3A=V1A3A+U1
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N1-n0,N1-n0,N0-n0,N0-n0/),'1423',IntR,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder_stripe(2,shape(t1A),size(t1A),'12',t1A,B2)
       allocate(U3(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,U3)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(V1A3A),size(V1A3A),'2134',
     & 1.000,V1A3A,U3)
       call sum_stripe(4,shape(V1A3A),size(V1A3A),'3124',
     & -1.000,V1A3A,U3)
       deallocate(U3)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N1-n0,N1-n0,N1-n0,N0-n0/),'1243',IntR,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_stripe(4,shape(t2A),size(t2A),'1234',t2A,D2)
       allocate(U5(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K1
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,U5)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V1A3A),size(V1A3A),'2314',
     & 0.500,V1A3A,U5)
       deallocate(U5)
C
       end
