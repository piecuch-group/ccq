       SUBROUTINE t4D_antisym(N0,N1,N2,N3,K1,K2,K3,K4,lvl,shift,
     & FockR,FockB,IntR,IntB,IntM,t1A,t1B,t2A,t2B,t2C,t3A,t3B,t3C,t3D,
     & iactocca,iactoccb,iactunoa,iactunob,iactindq)
!    & t4A,t4B,t4C,t4D,t4E)
C
       INTEGER a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p
       INTEGER iactocca,iactoccb,iactunoa,iactunob,iactindq
       INTEGER iocca,ioccb,iunoa,iunob
       INTEGER, ALLOCATABLE:: inDOcc(:,:,:,:)
       INTEGER, ALLOCATABLE:: indunocc(:,:,:,:)
       REAL*8 shift,PP,Coeleft,timt1,timt2
       REAL*8 FockR(N3,N3)
       REAL*8 FockB(N3,N3)
       REAL*8 sum
       REAL*8 IntR(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       REAL*8 IntB(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       REAL*8 IntM(N0+1:N3,N0+1:N3,N0+1:N3,N0+1:N3)
       REAL*8 t1A(N1+1:N3,N0+1:N1)
       REAL*8 t1B(N2+1:N3,N0+1:N2)
       REAL*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       REAL*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       REAL*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       REAL*8 t3A(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       REAL*8 t3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
       REAL*8 t3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
       REAL*8 t3D(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
!       REAL*8,ALLOCATABLE::T4A(:,:,:,:,:,:,:,:)
!       REAL*8 t4A(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3,
!     & N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1)
!       REAL*8,ALLOCATABLE::T4B(:,:,:,:,:,:,:,:)
!       REAL*8 t4B(N2+1:N3,N1+1:N3,N1+1:N3,N1+1:N3,
!     & N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N1)
       REAL*8,ALLOCATABLE::T4C(:,:,:,:,:,:,:,:)
!       REAL*8 t4C(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3,
!     & N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1)
       REAL*8,ALLOCATABLE::T4D(:,:,:,:,:,:,:,:)
!       REAL*8 t4D(N2+1:N3,N2+1:N3,N2+1:N3,N1+1:N3,
!     & N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N1)
       REAL*8,ALLOCATABLE::T4E(:,:,:,:,:,:,:,:)
!       REAL*8 t4E(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3,
!     & N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2)
       REAL*8,ALLOCATABLE::V4D(:,:,:,:,:,:,:,:)
!       REAL*8 V4D(N2+1:N3,N2+1:N3,N2+1:N3,N1+1:N3,
!     & N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N1)
C
       REAL*8,ALLOCATABLE::B1(:,:)
       REAL*8,ALLOCATABLE::B2(:,:)
       REAL*8,ALLOCATABLE::D1(:,:,:,:)
       REAL*8,ALLOCATABLE::D2(:,:,:,:)
       REAL*8,ALLOCATABLE::F1(:,:,:,:,:,:)
       REAL*8,ALLOCATABLE::F2(:,:,:,:,:,:)
       REAL*8,ALLOCATABLE::H2(:,:,:,:,:,:,:,:)
C
       INTEGER ta,tb,tc,td,te
       PARAMETER(ta=29,tb=30,tc=31,td=32,te=33)
C
       ALLOCATE(t4D(N2+1:N3,N2+1:N3,N2+1:N3,N1+1:N3,
     & N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N1))
C
       REWIND(td)
       READ(td)t4d
C
C
       DO i=N0+1,N1
       DO j=N0+1,N2-2
       DO k=j+1,N2-1
       DO l=k+1,N2
       DO a=N1+1,N3
       DO b=N2+1,N3-2
       DO c=b+1,N3-1
       DO d=c+1,N3
!
         t4D(d,c,b,a,l,j,k,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(d,c,b,a,j,k,l,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(d,c,b,a,j,l,k,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(d,c,b,a,k,l,j,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(d,c,b,a,k,j,l,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(d,b,c,a,l,k,j,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(d,b,c,a,l,j,k,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(d,b,c,a,j,k,l,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(d,b,c,a,j,l,k,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(d,b,c,a,k,l,j,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(d,b,c,a,k,j,l,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(b,c,d,a,l,k,j,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(b,c,d,a,l,j,k,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(b,c,d,a,j,k,l,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(b,c,d,a,j,l,k,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(b,c,d,a,k,l,j,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(b,c,d,a,k,j,l,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(b,d,c,a,l,k,j,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(b,d,c,a,l,j,k,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(b,d,c,a,j,k,l,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(b,d,c,a,j,l,k,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(b,d,c,a,k,l,j,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(b,d,c,a,k,j,l,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(c,d,b,a,l,k,j,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(c,d,b,a,l,j,k,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(c,d,b,a,j,k,l,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(c,d,b,a,j,l,k,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(c,d,b,a,k,l,j,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(c,d,b,a,k,j,l,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(c,b,d,a,l,k,j,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(c,b,d,a,l,j,k,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(c,b,d,a,j,k,l,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(c,b,d,a,j,l,k,i)= t4D(d,c,b,a,l,k,j,i)
         t4D(c,b,d,a,k,l,j,i)=-t4D(d,c,b,a,l,k,j,i)
         t4D(c,b,d,a,k,j,l,i)= t4D(d,c,b,a,l,k,j,i)
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       REWIND(td)
       WRITE(td)t4d
	CALL CPU_TIME(timt2)
C
       end
