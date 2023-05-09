       subroutine L3D_update(N0,N1,N2,N3,V3D,
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
       real*8 V3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
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
C
       do i=N0+1,N2;do j=N0+1,N2;do k=N0+1,N2
       do a=N2+1,N3;do b=N2+1,N3;do c=N2+1,N3
        V3D(c,b,a,k,j,i)=+H1B(c,i)*l2C(b,a,k,j)
     &                   -H1B(b,i)*l2C(c,a,k,j)
     &                   +H1B(a,i)*l2C(c,b,k,j)
     &                   -H1B(c,j)*l2C(b,a,k,i)
     &                   +H1B(b,j)*l2C(c,a,k,i)
     &                   -H1B(a,j)*l2C(c,b,k,i)
     &                   +H1B(c,k)*l2C(b,a,j,i)
     &                   -H1B(b,k)*l2C(c,a,j,i)
     &                   +H1B(a,k)*l2C(c,b,j,i)
     &                   +l1B(c,i)*H2C(b,a,k,j)
     &                   -l1B(b,i)*H2C(c,a,k,j)
     &                   +l1B(a,i)*H2C(c,b,k,j)
     &                   -l1B(c,j)*H2C(b,a,k,i)
     &                   +l1B(b,j)*H2C(c,a,k,i)
     &                   -l1B(a,j)*H2C(c,b,k,i)
     &                   +l1B(c,k)*H2C(b,a,j,i)
     &                   -l1B(b,k)*H2C(c,a,j,i)
     &                   +l1B(a,k)*H2C(c,b,j,i)
       enddo;enddo;enddo;enddo;enddo;enddo
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_shift(4,shape(H2C),size(H2C),shape(D1),size(D1),
     & (/N0-n0,N2-n0,N0-n0,N0-n0/),'1234',H2C,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_stripe(4,shape(l2C),size(l2C),'3124',l2C,D2)
       allocate(U1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2,N0+1:N2))
       I1=K2*K2*K4
       I2=K2*K4*K4
       I3=K2
       call EGEMM(I1,I2,I3,D1,D2,U1)
       deallocate(D1)
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
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2))
       call reorder_shift(4,shape(H2C),size(H2C),shape(D1),size(D1),
     & (/N2-n0,N2-n0,N2-n0,N0-n0/),'3124',H2C,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder_stripe(4,shape(l2C),size(l2C),'1234',l2C,D2)
       allocate(U2(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
       I1=K2*K4*K4
       I2=K2*K2*K4
       I3=K4
       call EGEMM(I1,I2,I3,D1,D2,U2)
       deallocate(D1)
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
C
       end
