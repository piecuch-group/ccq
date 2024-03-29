       subroutine L2A_update(N0,N1,N2,N3,V2A,
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
       real*8 V2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
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
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(l2A),size(l2A),'3412',l2A,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(t2A),size(t2A),'3412',t2A,D2)
       allocate(Q1(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q1)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N1-n0,N1-n0,N0-n0,N0-n0/),'1234',IntR,D1)
       allocate(U8(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K3
       call EGEMM(I1,I2,I3,D1,Q1,U8)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2134',0.500,
     & V2A,U8)
       V2A=V2A-1.0d0/2*U8
       deallocate(U8)
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2A),size(l2A),'3124',l2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(t2A),size(t2A),'3124',t2A,D2)
       allocate(Q2(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q2)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N1-n0,N0-n0/),'3124',IntR,D1)
       allocate(U9(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K1
       call EGEMM(I1,I2,I3,D1,Q2,U9)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2A),size(V2A),'3124',-0.500,
     & V2A,U9)
       call sum_stripe(4,shape(V2A),size(V2A),'4123',0.500,
     & V2A,U9)
       deallocate(U9)
       deallocate(Q2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(l2B),size(l2B),'3412',l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(t2B),size(t2B),'3412',t2B,D2)
       allocate(Q3(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q3)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N1-n0,N1-n0,N0-n0,N0-n0/),'1234',IntR,D1)
       allocate(U11(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K3
       call EGEMM(I1,I2,I3,D1,Q3,U11)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2134',1.000,
     & V2A,U11)
       V2A=V2A-U11
       deallocate(U11)
       deallocate(Q3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2B),size(l2B),'3124',l2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(t2B),size(t2B),'3124',t2B,D2)
       allocate(Q4(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q4)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntR),size(IntR),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N1-n0,N0-n0/),'3124',IntR,D1)
       allocate(U12(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K1
       call EGEMM(I1,I2,I3,D1,Q4,U12)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2A),size(V2A),'3124',-1.000,
     & V2A,U12)
       call sum_stripe(4,shape(V2A),size(V2A),'4123',1.000,
     & V2A,U12)
       deallocate(U12)
       deallocate(Q4)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(H2A),size(H2A),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N0-n0,N0-n0/),'1234',H2A,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(l1A),size(l1A),'21',l1A,B2)
       allocate(U1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,U1)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2134',1.000,
     & V2A,U1)
       V2A=V2A-U1
       deallocate(U1)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(H2A),size(H2A),shape(D1),size(D1),
     & (/N1-n0,N1-n0,N1-n0,N0-n0/),'3124',H2A,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder_stripe(2,shape(l1A),size(l1A),'12',l1A,B2)
       allocate(U2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,U2)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'3124',1.000,
     & V2A,U2)
       call sum_stripe(4,shape(V2A),size(V2A),'4123',-1.000,
     & V2A,U2)
       deallocate(U2)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call reorder_shift(2,shape(H1A),size(H1A),shape(B1),size(B1),
     & (/N0-n0,N0-n0/),'12',H1A,B1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2A),size(l2A),'3124',l2A,D2)
       allocate(U3(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1*K3*K3
       I3=K1
       call EGEMM(I1,I2,I3,B1,D2,U3)
       deallocate(B1)
       deallocate(D2)
C
       V2A=V2A+U3
       call sum_stripe(4,shape(V2A),size(V2A),'1243',-1.000,
     & V2A,U3)
       deallocate(U3)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call reorder_shift(2,shape(H1A),size(H1A),shape(B1),size(B1),
     & (/N1-n0,N1-n0/),'21',H1A,B1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_stripe(4,shape(l2A),size(l2A),'1234',l2A,D2)
       allocate(U4(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K1*K3
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,U4)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2341',1.000,
     & V2A,U4)
       call sum_stripe(4,shape(V2A),size(V2A),'1342',-1.000,
     & V2A,U4)
       deallocate(U4)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(H2A),size(H2A),shape(D1),size(D1),
     & (/N0-n0,N0-n0,N0-n0,N0-n0/),'1234',H2A,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(l2A),size(l2A),'3412',l2A,D2)
       allocate(U5(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K3*K3
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,U5)
       deallocate(D1)
       deallocate(D2)
C
       V2A=V2A+1.0d0/2*U5
       deallocate(U5)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(H2A),size(H2A),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N1-n0,N0-n0/),'1324',H2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2A),size(l2A),'3124',l2A,D2)
       allocate(U6(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,U6)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2314',-1.000,
     & V2A,U6)
       call sum_stripe(4,shape(V2A),size(V2A),'1324',1.000,
     & V2A,U6)
       call sum_stripe(4,shape(V2A),size(V2A),'2413',1.000,
     & V2A,U6)
       call sum_stripe(4,shape(V2A),size(V2A),'1423',-1.000,
     & V2A,U6)
       deallocate(U6)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3))
       call reorder_shift(4,shape(H2A),size(H2A),shape(D1),size(D1),
     & (/N1-n0,N1-n0,N1-n0,N1-n0/),'3412',H2A,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_stripe(4,shape(l2A),size(l2A),'1234',l2A,D2)
       allocate(U7(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3
       I2=K1*K1
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,U7)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'3412',0.500,
     & V2A,U7)
       deallocate(U7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N1-n0,N0-n0/),'1324',H2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2B),size(l2B),'3124',l2B,D2)
       allocate(U10(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K1*K3
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,U10)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2A),size(V2A),'2314',-1.000,
     & V2A,U10)
       call sum_stripe(4,shape(V2A),size(V2A),'1324',1.000,
     & V2A,U10)
       call sum_stripe(4,shape(V2A),size(V2A),'2413',1.000,
     & V2A,U10)
       call sum_stripe(4,shape(V2A),size(V2A),'1423',-1.000,
     & V2A,U10)
       deallocate(U10)
C
       end
