       subroutine L2B_update(N0,N1,N2,N3,V2B,
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
       real*8 V2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
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
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
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
       real*8,allocatable::U13(:,:,:,:)
       real*8,allocatable::U14(:,:,:,:)
       real*8,allocatable::U15(:,:,:,:)
       real*8,allocatable::U16(:,:,:,:)
       real*8,allocatable::U17(:,:,:,:)
       real*8,allocatable::U18(:,:,:,:)
       real*8,allocatable::U19(:,:,:,:)
       real*8,allocatable::U20(:,:,:,:)
       real*8,allocatable::U21(:,:,:,:)
       real*8,allocatable::U22(:,:,:,:)
       real*8,allocatable::U23(:,:,:,:)
       real*8,allocatable::U24(:,:,:,:)
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
       allocate(D1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N1-n0,N2-n0,N0-n0,N0-n0/),'2134',IntM,D1)
       allocate(U6(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K3
       I3=K3
       call EGEMM(I1,I2,I3,D1,Q1,U6)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2B),size(V2B),'2134',-0.500,
     & V2B,U6)
       deallocate(U6)
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
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N1-n0,N0-n0/),'4123',IntM,D1)
       allocate(U7(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1
       I3=K1
       call EGEMM(I1,I2,I3,D1,Q2,U7)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2B),size(V2B),'4123',-0.500,
     & V2B,U7)
       deallocate(U7)
       deallocate(Q2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(l2B),size(l2B),'3421',l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(t2B),size(t2B),'3421',t2B,D2)
       allocate(Q3(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q3)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N2-n0,N1-n0,N0-n0,N0-n0/),'1234',IntM,D1)
       allocate(U18(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K4
       I3=K4
       call EGEMM(I1,I2,I3,D1,Q3,U18)
       deallocate(D1)
C
       V2B=V2B-U18
       deallocate(U18)
       deallocate(Q3)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2B),size(l2B),'4123',l2B,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2B),size(t2B),'4123',t2B,D2)
       allocate(Q4(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q4)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N1-n0,N0-n0/),'3124',IntM,D1)
       allocate(U19(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K2
       call EGEMM(I1,I2,I3,D1,Q4,U19)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2B),size(V2B),'3124',-1.000,
     & V2B,U19)
       deallocate(U19)
       deallocate(Q4)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(l2B),size(l2B),'3412',l2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(t2B),size(t2B),'3412',t2B,D2)
       allocate(Q5(N1+1:N3,N1+1:N3))
       I1=K3
       I2=K3
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q5)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N1-n0,N2-n0,N0-n0,N0-n0/),'2134',IntM,D1)
       allocate(U20(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K3
       I3=K3
       call EGEMM(I1,I2,I3,D1,Q5,U20)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2B),size(V2B),'2134',-1.000,
     & V2B,U20)
       deallocate(U20)
       deallocate(Q5)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2B),size(l2B),'3124',l2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(t2B),size(t2B),'3124',t2B,D2)
       allocate(Q6(N0+1:N1,N0+1:N1))
       I1=K1
       I2=K1
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q6)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N1-n0,N0-n0/),'4123',IntM,D1)
       allocate(U21(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1
       I3=K1
       call EGEMM(I1,I2,I3,D1,Q6,U21)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2B),size(V2B),'4123',-1.000,
     & V2B,U21)
       deallocate(U21)
       deallocate(Q6)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(l2C),size(l2C),'3412',l2C,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder_stripe(4,shape(t2C),size(t2C),'3412',t2C,D2)
       allocate(Q7(N2+1:N3,N2+1:N3))
       I1=K4
       I2=K4
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q7)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N2-n0,N1-n0,N0-n0,N0-n0/),'1234',IntM,D1)
       allocate(U23(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K4
       I3=K4
       call EGEMM(I1,I2,I3,D1,Q7,U23)
       deallocate(D1)
C
       V2B=V2B-1.0d0/2*U23
       deallocate(U23)
       deallocate(Q7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2C),size(l2C),'3124',l2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(t2C),size(t2C),'3124',t2C,D2)
       allocate(Q8(N0+1:N2,N0+1:N2))
       I1=K2
       I2=K2
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q8)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N1-n0,N0-n0/),'3124',IntM,D1)
       allocate(U24(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K2
       call EGEMM(I1,I2,I3,D1,Q8,U24)
       deallocate(D1)
C
       call sum_stripe(4,shape(V2B),size(V2B),'3124',-0.500,
     & V2B,U24)
       deallocate(U24)
       deallocate(Q8)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N0-n0,N0-n0/),'2134',H2B,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder_stripe(2,shape(l1A),size(l1A),'21',l1A,B2)
       allocate(U1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,U1)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(V2B),size(V2B),'2134',-1.000,
     & V2B,U1)
       deallocate(U1)
C
       allocate(D1(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N1-n0,N2-n0,N1-n0,N0-n0/),'4123',H2B,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder_stripe(2,shape(l1A),size(l1A),'12',l1A,B2)
       allocate(U2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,U2)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(V2B),size(V2B),'4123',1.000,
     & V2B,U2)
       deallocate(U2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N0-n0,N0-n0/),'1234',H2B,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder_stripe(2,shape(l1B),size(l1B),'21',l1B,B2)
       allocate(U3(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,U3)
       deallocate(D1)
       deallocate(B2)
C
       V2B=V2B-U3
       deallocate(U3)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N1-n0,N0-n0/),'3124',H2B,D1)
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(l1B),size(l1B),'12',l1B,B2)
       allocate(U4(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,U4)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(V2B),size(V2B),'3124',1.000,
     & V2B,U4)
       deallocate(U4)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N2-n0,N0-n0/),'2413',H2B,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2A),size(l2A),'3124',l2A,D2)
       allocate(U5(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,U5)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2B),size(V2B),'2413',1.000,
     & V2B,U5)
       deallocate(U5)
C
       allocate(B1(N0+1:N1,N0+1:N1))
       call reorder_shift(2,shape(H1A),size(H1A),shape(B1),size(B1),
     & (/N0-n0,N0-n0/),'12',H1A,B1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2B),size(l2B),'4123',l2B,D2)
       allocate(U8(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1
       I2=K2*K3*K4
       I3=K1
       call EGEMM(I1,I2,I3,B1,D2,U8)
       deallocate(B1)
       deallocate(D2)
C
       V2B=V2B-U8
       deallocate(U8)
C
       allocate(B1(N1+1:N3,N1+1:N3))
       call reorder_shift(2,shape(H1A),size(H1A),shape(B1),size(B1),
     & (/N1-n0,N1-n0/),'21',H1A,B1)
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder_stripe(4,shape(l2B),size(l2B),'2134',l2B,D2)
       allocate(U9(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3
       I2=K1*K2*K4
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,U9)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2B),size(V2B),'1342',1.000,
     & V2B,U9)
       deallocate(U9)
C
       allocate(B1(N0+1:N2,N0+1:N2))
       call reorder_shift(2,shape(H1B),size(H1B),shape(B1),size(B1),
     & (/N0-n0,N0-n0/),'12',H1B,B1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2B),size(l2B),'3124',l2B,D2)
       allocate(U10(N2+1:N3,N1+1:N3,N0+1:N1,N0+1:N2))
       I1=K2
       I2=K1*K3*K4
       I3=K2
       call EGEMM(I1,I2,I3,B1,D2,U10)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2B),size(V2B),'1243',-1.000,
     & V2B,U10)
       deallocate(U10)
C
       allocate(B1(N2+1:N3,N2+1:N3))
       call reorder_shift(2,shape(H1B),size(H1B),shape(B1),size(B1),
     & (/N2-n0,N2-n0/),'21',H1B,B1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder_stripe(4,shape(l2B),size(l2B),'1234',l2B,D2)
       allocate(U11(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4
       I2=K1*K2*K3
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,U11)
       deallocate(B1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2B),size(V2B),'2341',1.000,
     & V2B,U11)
       deallocate(U11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(H2A),size(H2A),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N1-n0,N0-n0/),'1324',H2A,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2B),size(l2B),'4213',l2B,D2)
       allocate(U12(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,U12)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2B),size(V2B),'1324',1.000,
     & V2B,U12)
       deallocate(U12)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N0-n0,N0-n0,N0-n0,N0-n0/),'1234',H2B,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder_stripe(4,shape(l2B),size(l2B),'3412',l2B,D2)
       allocate(U13(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K3*K4
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,U13)
       deallocate(D1)
       deallocate(D2)
C
       V2B=V2B+U13
       deallocate(U13)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:N3,N0+1:N1))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N2-n0,N0-n0/),'2314',H2B,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2B),size(l2B),'4123',l2B,D2)
       allocate(U14(N1+1:N3,N0+1:N2,N2+1:N3,N0+1:N1))
       I1=K1*K4
       I2=K2*K3
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,U14)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2B),size(V2B),'2314',-1.000,
     & V2B,U14)
       deallocate(U14)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:N3,N0+1:N2))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N1-n0,N0-n0/),'1423',H2B,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2B),size(l2B),'3214',l2B,D2)
       allocate(U15(N2+1:N3,N0+1:N1,N1+1:N3,N0+1:N2))
       I1=K2*K3
       I2=K1*K4
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,U15)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2B),size(V2B),'1423',-1.000,
     & V2B,U15)
       deallocate(U15)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:N3))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N2-n0,N1-n0,N2-n0,N1-n0/),'3412',H2B,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder_stripe(4,shape(l2B),size(l2B),'1234',l2B,D2)
       allocate(U16(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4
       I2=K1*K2
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,U16)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2B),size(V2B),'3412',1.000,
     & V2B,U16)
       deallocate(U16)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(H2C),size(H2C),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N2-n0,N0-n0/),'1324',H2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2B),size(l2B),'3124',l2B,D2)
       allocate(U17(N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2))
       I1=K2*K4
       I2=K1*K3
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,U17)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2B),size(V2B),'2413',1.000,
     & V2B,U17)
       deallocate(U17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N1-n0,N0-n0/),'1324',H2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2C),size(l2C),'3124',l2C,D2)
       allocate(U22(N2+1:N3,N0+1:N2,N1+1:N3,N0+1:N1))
       I1=K1*K3
       I2=K2*K4
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,U22)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V2B),size(V2B),'1324',1.000,
     & V2B,U22)
       deallocate(U22)
C
       end
