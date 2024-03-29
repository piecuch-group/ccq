       subroutine HBar0A4C(N0,N1,N2,N3,V0A4C,
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
       real*8 V0A4C(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::U1(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::U2(:,:,:,:)
       real*8,allocatable::U3(:,:,:,:)
C
       allocate(D1(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N1-n0,N2-n0,N0-n0,N0-n0/),'2134',IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder_stripe(2,shape(t1A),size(t1A),'12',t1A,B2)
       allocate(S4(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
       X1=0.0d0
       call sum_stripe(4,shape(X1),size(X1),'4123',1.000,X1,
     & S4)
       deallocate(S4)
C
       call sumx1423(N0,N3,N2,N3,N0,N2,N0,N1,N0,N1,X1,IntM, 1.000)
C
       allocate(B2(N2+1:N3,N0+1:N2))
       call reorder_stripe(2,shape(t1B),size(t1B),'12',t1B,B2)
       allocate(U2(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
       I1=K1*K1*K2
       I2=K2
       I3=K4
       call EGEMM(I1,I2,I3,X1,B2,U2)
       deallocate(B2)
C
       call sum_stripe(4,shape(V0A4C),size(V0A4C),'2134',
     & 1.000,V0A4C,U2)
       deallocate(U2)
       deallocate(X1)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N1-n0,N0-n0,N0-n0,N0-n0/),'2314',IntM,D1)
       allocate(B2(N1+1:N3,N0+1:N1))
       call reorder_stripe(2,shape(t1A),size(t1A),'12',t1A,B2)
       allocate(U1(N0+1:N1,N0+1:N2,N0+1:N2,N0+1:N1))
       I1=K1*K2*K2
       I2=K1
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,U1)
       deallocate(D1)
       deallocate(B2)
C
       call sum_stripe(4,shape(V0A4C),size(V0A4C),'4123',
     & 1.000,V0A4C,U1)
       deallocate(U1)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(IntM),size(IntM),shape(D1),size(D1),
     & (/N2-n0,N1-n0,N0-n0,N0-n0/),'1234',IntM,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder_stripe(4,shape(t2B),size(t2B),'1234',t2B,D2)
       allocate(U3(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K1*K2
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,U3)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(4,shape(V0A4C),size(V0A4C),'2413',
     & 1.000,V0A4C,U3)
       deallocate(U3)
C
       end
