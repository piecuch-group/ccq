       subroutine L2C_update(N0,N1,N2,N3,V2C,
     & K1,K2,K3,K4,
     & FockR,FockB,IntR,IntB,IntM,
     & H1A,H1B,H2A,H2B,H2C,
     & t1A,t1B,t2A,t2B,t2C,
     & l1A,l1B,l2A,l2B,l2C)
C
       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 FockR(N3,N3)
       real*8 FockB(N3,N3)
       real*8 IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H1A(N0+1:N3,N0+1:N3)
       real*8 H1B(N0+1:N3,N0+1:N3)
       real*8 H2A(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2B(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 H2C(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 l1A(N1+1:N3,N0+1:N1)
       real*8 l1B(N2+1:N3,N0+1:N2)
       real*8 l2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 l2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 l2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
C
       real*8 V2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
C
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:)
       real*8,allocatable::U3(:,:,:,:)
       real*8,allocatable::U4(:,:,:,:)
       real*8,allocatable::U5(:,:,:,:)
       real*8,allocatable::U6(:,:,:,:)
       real*8,allocatable::U7(:,:,:,:)
       real*8,allocatable::U8(:,:,:,:)
       real*8,allocatable::U9(:,:,:,:)
       real*8,allocatable::U10(:,:,:,:)
       real*8,allocatable::U11(:,:,:,:)
       real*8,allocatable::U12(:,:,:,:)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(l2B),size(l2B),'3421',l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(t2B),size(t2B),'3421',t2B,D2)
       allocate(Q1(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q1)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N0-n0,N0-n0/),'1234',IntB,D1)
       allocate(U4(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K4
       call EGEMM(I1,I2,I3,D1,Q1,U4)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2C),size(V2C),'2134',1.000,
     & V2C,U4)
       V2C=V2C-U4
       deallocate(U4)
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2B),size(l2B),'4123',l2B,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2B),size(t2B),'4123',t2B,D2)
       allocate(Q2(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q2)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N2-n0,N0-n0/),'3124',IntB,D1)
       allocate(U5(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K2
       call EGEMM(I1,I2,I3,D1,Q2,U5)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2C),size(V2C),'3124',-1.000,
     & V2C,U5)
       call sum_stripe(4,shape(V2C),size(V2C),'4123',1.000,
     & V2C,U5)
       deallocate(U5)
       deallocate(Q2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(l2C),size(l2C),'3412',l2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(t2C),size(t2C),'3412',t2C,D2)
       allocate(Q3(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q3)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N0-n0,N0-n0/),'1234',IntB,D1)
       allocate(U11(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K4
       call EGEMM(I1,I2,I3,D1,Q3,U11)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2C),size(V2C),'2134',0.500,
     & V2C,U11)
       V2C=V2C-1.0d0/2*U11
       deallocate(U11)
       deallocate(Q3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2C),size(l2C),'3124',l2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(Q4(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q4)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(IntB),size(IntB),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N2-n0,N0-n0/),'3124',IntB,D1)
       allocate(U12(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K2
       call EGEMM(I1,I2,I3,D1,Q4,U12)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2C),size(V2C),'3124',-0.500,
     & V2C,U12)
       call sum_stripe(4,shape(V2C),size(V2C),'4123',0.500,
     & V2C,U12)
       deallocate(U12)
       deallocate(Q4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(H2C),size(H2C),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N0-n0,N0-n0/),'1234',H2C,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(l1B),size(l1B),'21',l1B,B2)
       allocate(U1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,U1)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(V2C),size(V2C),'2134',1.000,
     & V2C,U1)
       V2C=V2C-U1
       deallocate(U1)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(H2C),size(H2C),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N2-n0,N0-n0/),'3124',H2C,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(l1B),size(l1B),'12',l1B,B2)
       allocate(U2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,U2)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(V2C),size(V2C),'3124',1.000,
     & V2C,U2)
       call sum_stripe(4,shape(V2C),size(V2C),'4123',-1.000,
     & V2C,U2)
       deallocate(U2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N2-n0,N0-n0/),'2413',H2B,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2B),size(l2B),'4213',l2B,D2)
       allocate(U3(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,U3)
       deallocate(D1)
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
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call reorder_shift(2,shape(H1B),size(H1B),shape(B1),size(B1),
     & (/N0-n0,N0-n0/),'12',H1B,B1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2C),size(l2C),'3124',l2C,D2)
       allocate(U6(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,B1,D2,U6)
       deallocate(B1)
       deallocate(D2)
C
       V2C=V2C+U6
       call sum_stripe(4,shape(V2C),size(V2C),'1243',-1.000,
     & V2C,U6)
       deallocate(U6)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call reorder_shift(2,shape(H1B),size(H1B),shape(B1),size(B1),
     & (/N2-n0,N2-n0/),'21',H1B,B1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(l2C),size(l2C),'1234',l2C,D2)
       allocate(U7(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,U7)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2C),size(V2C),'2341',1.000,
     & V2C,U7)
       call sum_stripe(4,shape(V2C),size(V2C),'1342',-1.000,
     & V2C,U7)
       deallocate(U7)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(H2C),size(H2C),shape(D1),size(D1),
     & (/N0-n0,N0-n0,N0-n0,N0-n0/),'1234',H2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(l2C),size(l2C),'3412',l2C,D2)
       allocate(U8(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K4*K4
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,U8)
       deallocate(D1)
       deallocate(D2)
C
       V2C=V2C+1.0d0/2*U8
       deallocate(U8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(H2C),size(H2C),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N2-n0,N0-n0/),'1324',H2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2C),size(l2C),'3124',l2C,D2)
       allocate(U9(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,U9)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2C),size(V2C),'2314',-1.000,
     & V2C,U9)
       call sum_stripe(4,shape(V2C),size(V2C),'1324',1.000,
     & V2C,U9)
       call sum_stripe(4,shape(V2C),size(V2C),'2413',1.000,
     & V2C,U9)
       call sum_stripe(4,shape(V2C),size(V2C),'1423',-1.000,
     & V2C,U9)
       deallocate(U9)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3))
       call reorder_shift(4,shape(H2C),size(H2C),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N2-n0,N2-n0/),'3412',H2C,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(l2C),size(l2C),'1234',l2C,D2)
       allocate(U10(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4
       I2=K2*K2
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,U10)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2C),size(V2C),'3412',0.500,
     & V2C,U10)
       deallocate(U10)
C
       end
