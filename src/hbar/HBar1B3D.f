       subroutine HBar1B3D(N0,N1,N2,N3,V1B3D,
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
       real*8 V1B3D(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
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
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::X1(:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::U8(:,:,:,:)
       real*8,allocatable::U9(:,:,:,:)
       real*8,allocatable::U11(:,:,:,:)
       real*8,allocatable::U13(:,:,:,:)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N2-n0,N0-n0/),'4213',IntM,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(t1A),size(t1A),'21',t1A,B2)
       allocate(Q1(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N2+1:N3,N0+1:N2))
       X1=0.0d0
       X1=X1+Q1
       deallocate(Q1)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N2-n0,N0-n0,N1-n0,N0-n0/),'1423',IntM,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S4(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3*K1
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N2))
       X2=0.0d0
       call sum_stripe(4,shape(X2),size(X2),'3124',1.000,X2,
     & S4)
       deallocate(S4)
C
       call sumx3241(N0,N3,N0,N1,N1,N3,N0,N2,N0,N2,X2,IntM, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2B),size(t2B),'4213',t2B,D2)
       allocate(U2(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,X2,D2,U2)
       deallocate(D2)
C
       V1B3D=V1B3D-U2
       call sum_stripe(4,shape(V1B3D),size(V1B3D),'1324',
     & 1.000,V1B3D,U2)
       deallocate(U2)
       deallocate(X2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N2-n0,N0-n0,N0-n0,N0-n0/),'1423',IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S10(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(S10),size(S10),'2134',S10,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(U11(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,U11)
       deallocate(D1)
       deallocate(B2)
C
       V1B3D=V1B3D-U11
       call sum_stripe(4,shape(V1B3D),size(V1B3D),'1324',
     & 1.000,V1B3D,U11)
       deallocate(U11)
       deallocate(S10)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N2-n0,N0-n0/),'2143',IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S12(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(S12),size(S12),'2314',S12,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(U13(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,U13)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(V1B3D),size(V1B3D),'2134',
     & 1.000,V1B3D,U13)
       deallocate(U13)
       deallocate(S12)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N2-n0,N0-n0,N2-n0,N0-n0/),'1423',IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S14(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       X4=0.0d0
       call sum_stripe(4,shape(X4),size(X4),'3124',-1.000,
     & X4,S14)
C
       call sumx2341(N0,N3,N0,N2,N2,N3,N0,N2,N0,N2,X4,IntB, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(U8(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,X4,D2,U8)
       deallocate(D2)
C
       V1B3D=V1B3D+U8
       call sum_stripe(4,shape(V1B3D),size(V1B3D),'1324',
     & -1.000,V1B3D,U8)
       deallocate(U8)
       deallocate(X4)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(S14),size(S14),'3214',S14,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(S19(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       X3=0.0d0
       call sum_stripe(4,shape(X3),size(X3),'2134',-1.000,
     & X3,S19)
       deallocate(S19)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N2-n0,N0-n0/),'3124',IntB,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(Q2(N2+1:N3,N0+1:N2))
       I1=K2*K4
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       X1=X1+Q2
       deallocate(Q2)
C
       call sumx12(0,N3,N2,N3,N0,N2,X1,FockB, 1.000)
C
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'1234',t2C,D2)
       allocate(U1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,X1,D2,U1)
       deallocate(D2)
C
       V1B3D=V1B3D+U1
       deallocate(U1)
       deallocate(X1)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N0-n0,N0-n0/),'1243',IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'1234',t2C,D2)
       allocate(S16(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K2*K2
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S16)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(X3),size(X3),'2314',0.500,X3,
     & S16)
       deallocate(S16)
C
       call sumx2341(N0,N3,N0,N2,N0,N2,N0,N2,N0,N2,X3,IntB, 1.000)
C
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(t1B),size(t1B),'21',t1B,B2)
       allocate(U6(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2*K2
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,X3,B2,U6)
       deallocate(B2)
C
       V1B3D=V1B3D-U6
       deallocate(U6)
       deallocate(X3)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N0-n0,N0-n0/),'1423',IntB,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(U7(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,U7)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(V1B3D),size(V1B3D),'2134',
     & 1.000,V1B3D,U7)
       call sum_stripe(4,shape(V1B3D),size(V1B3D),'3124',
     & -1.000,V1B3D,U7)
       deallocate(U7)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N2-n0,N0-n0/),'1243',IntB,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'1234',t2C,D2)
       allocate(U9(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K2
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,U9)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V1B3D),size(V1B3D),'2314',
     & 0.500,V1B3D,U9)
       deallocate(U9)
C
       end
