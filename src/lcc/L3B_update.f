       subroutine L3B_update(N0,N1,N2,N3,V3B,
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
       real*8 V3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
C
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F1(:,:,:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
C
       real*8,allocatable::U1(:,:,:,:,:,:)
       real*8,allocatable::U2(:,:,:,:,:,:)
       real*8,allocatable::U3(:,:,:,:,:,:)
       real*8,allocatable::U4(:,:,:,:,:,:)
       real*8,allocatable::U5(:,:,:,:,:,:)
       real*8,allocatable::U6(:,:,:,:,:,:)
C
       do i=N0+1,N1;do j=N0+1,N1;do k=N0+1,N2
       do a=N1+1,N3;do b=N1+1,N3;do c=N2+1,N3
        V3B(c,b,a,k,j,i)=+H1B(c,k)*l2A(b,a,j,i)
     &                   -H1A(b,i)*l2B(c,a,k,j)
     &                   +H1A(a,i)*l2B(c,b,k,j)
     &                   +H1A(b,j)*l2B(c,a,k,i)
     &                   -H1A(a,j)*l2B(c,b,k,i)
     &                   +l1B(c,k)*H2A(b,a,j,i)
     &                   -l1A(b,i)*H2B(c,a,k,j)
     &                   +l1A(a,i)*H2B(c,b,k,j)
     &                   +l1A(b,j)*H2B(c,a,k,i)
     &                   -l1A(a,j)*H2B(c,b,k,i)
       enddo;enddo;enddo;enddo;enddo;enddo
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N0-n0,N0-n0/),'2134',H2B,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2A),size(l2A),'3124',l2A,D2)
       allocate(U1(N1+1:N3,N1+1:N3,N0+1:N1,N2+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K4
       I2=K1*K3*K3
       I3=K1
       call EGEMM(I1,I2,I3,D1,D2,U1)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'235146',
     & 1.000,V3B,U1)
       call sum_stripe(6,shape(V3B),size(V3B),'236145',
     & -1.000,V3B,U1)
       deallocate(U1)
C
       allocate(D1(N1+1:N3,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N1-n0,N2-n0,N1-n0,N0-n0/),'4123',H2B,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_stripe(4,shape(l2A),size(l2A),'1234',l2A,D2)
       allocate(U2(N1+1:N3,N0+1:N1,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       I1=K2*K3*K4
       I2=K1*K1*K3
       I3=K3
       call EGEMM(I1,I2,I3,D1,D2,U2)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'356124',
     & 1.000,V3B,U2)
       call sum_stripe(6,shape(V3B),size(V3B),'256134',
     & -1.000,V3B,U2)
       deallocate(U2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder_shift(4,shape(H2A),size(H2A),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N0-n0,N0-n0/),'1234',H2A,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2B),size(l2B),'4123',l2B,D2)
       allocate(U3(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3,N0+1:N1,N0+1:N1))
       I1=K1*K1*K3
       I2=K2*K3*K4
       I3=K1
       call EGEMM(I1,I2,I3,D1,D2,U3)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'134256',
     & 1.000,V3B,U3)
       call sum_stripe(6,shape(V3B),size(V3B),'124356',
     & -1.000,V3B,U3)
       deallocate(U3)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(H2A),size(H2A),shape(D1),size(D1),
     & (/N1-n0,N1-n0,N1-n0,N0-n0/),'3124',H2A,D1)
       allocate(D2(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
       call reorder_stripe(4,shape(l2B),size(l2B),'2134',l2B,D2)
       allocate(U4(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K3
       I2=K1*K2*K4
       I3=K3
       call EGEMM(I1,I2,I3,D1,D2,U4)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'145236',
     & 1.000,V3B,U4)
       call sum_stripe(6,shape(V3B),size(V3B),'146235',
     & -1.000,V3B,U4)
       deallocate(U4)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N0-n0,N1-n0,N0-n0,N0-n0/),'1234',H2B,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_stripe(4,shape(l2B),size(l2B),'3124',l2B,D2)
       allocate(U5(N2+1:N3,N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N2,N0+1:N1))
       I1=K1*K2*K3
       I2=K1*K3*K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,D2,U5)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'135246',
     & 1.000,V3B,U5)
       call sum_stripe(6,shape(V3B),size(V3B),'125346',
     & -1.000,V3B,U5)
       call sum_stripe(6,shape(V3B),size(V3B),'136245',
     & -1.000,V3B,U5)
       call sum_stripe(6,shape(V3B),size(V3B),'126345',
     & 1.000,V3B,U5)
       deallocate(U5)
C
       allocate(D1(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N1))
       call reorder_shift(4,shape(H2B),size(H2B),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N1-n0,N0-n0/),'3124',H2B,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder_stripe(4,shape(l2B),size(l2B),'1234',l2B,D2)
       allocate(U6(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N1))
       I1=K1*K3*K4
       I2=K1*K2*K3
       I3=K4
       call EGEMM(I1,I2,I3,D1,D2,U6)
       deallocate(D1)
       deallocate(D2)
C
       call sum_stripe(6,shape(V3B),size(V3B),'345126',
     & -1.000,V3B,U6)
       call sum_stripe(6,shape(V3B),size(V3B),'245136',
     & 1.000,V3B,U6)
       call sum_stripe(6,shape(V3B),size(V3B),'346125',
     & 1.000,V3B,U6)
       call sum_stripe(6,shape(V3B),size(V3B),'246135',
     & -1.000,V3B,U6)
       deallocate(U6)
C
       end
