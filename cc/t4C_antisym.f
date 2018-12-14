       SUBROUTINE t4C_antisym(N0,N1,N2,N3,K1,K2,K3,K4,lvl,shift,
     & FockR,FockB,IntR,IntB,IntM,t1A,t1B,t2A,t2B,t2C,t3A,t3B,t3C,t3D,
     & iactocca,iactoccb,iactunoa,iactunob,iactindq)
!    & t4A,t4B,t4C,t4D,t4E)
C
       INTEGER a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p
       INTEGER iactocca,iactoccb,iactunoa,iactunob,iactindq
       INTEGER iocca,ioccb,iunoa,iunob
!       INTEGER inDOcc(N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1)
!       INTEGER indunocc(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3)
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
       REAL*8,ALLOCATABLE::T4A(:,:,:,:,:,:,:,:)
!       REAL*8 t4A(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3,
!     & N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1)
       REAL*8,ALLOCATABLE::T4B(:,:,:,:,:,:,:,:)
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
       REAL*8,ALLOCATABLE::V4C(:,:,:,:,:,:,:,:)
!       REAL*8 V4C(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,
!     & N0+1:N1,N0+1:N1)
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
       ALLOCATE(t4C(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3,
     & N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1))
C
       REWIND(tc)
       READ(tc)t4c
C
C
       DO i=N0+1,N1-1
       DO j=i+1,N1
       DO k=N0+1,N2-1
       DO l=k+1,N2
       DO a=N1+1,N3-1
       DO b=a+1,N3
       DO c=N2+1,N3-1
       DO d=c+1,N3
!
         t4C(d,c,b,a,l,k,i,j)=-t4C(d,c,b,a,l,k,j,i)
         t4C(d,c,b,a,k,l,j,i)=-t4C(d,c,b,a,l,k,j,i)
         t4C(d,c,b,a,k,l,i,j)= t4C(d,c,b,a,l,k,j,i)
         t4C(d,c,a,b,l,k,j,i)=-t4C(d,c,b,a,l,k,j,i)
         t4C(d,c,a,b,l,k,i,j)= t4C(d,c,b,a,l,k,j,i)
         t4C(d,c,a,b,k,l,j,i)= t4C(d,c,b,a,l,k,j,i)
         t4C(d,c,a,b,k,l,i,j)=-t4C(d,c,b,a,l,k,j,i)
         t4C(c,d,b,a,l,k,j,i)=-t4C(d,c,b,a,l,k,j,i)
         t4C(c,d,b,a,l,k,i,j)= t4C(d,c,b,a,l,k,j,i)
         t4C(c,d,b,a,k,l,j,i)= t4C(d,c,b,a,l,k,j,i)
         t4C(c,d,b,a,k,l,i,j)=-t4C(d,c,b,a,l,k,j,i)
         t4C(c,d,a,b,l,k,j,i)= t4C(d,c,b,a,l,k,j,i)
         t4C(c,d,a,b,l,k,i,j)=-t4C(d,c,b,a,l,k,j,i)
         t4C(c,d,a,b,k,l,j,i)=-t4C(d,c,b,a,l,k,j,i)
         t4C(c,d,a,b,k,l,i,j)= t4C(d,c,b,a,l,k,j,i)
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       REWIND(tc)
       WRITE(tc)t4c
	CALL CPU_TIME(timt2)
C
       end
