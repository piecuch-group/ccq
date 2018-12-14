       SUBROUTINE t4B_antisym(N0,N1,N2,N3,K1,K2,K3,K4,lvl,shift,
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
       REAL*8,ALLOCATABLE::t4A(:,:,:,:,:,:,:,:)
!       REAL*8 t4A(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:N3,
!     & N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1)
       REAL*8,ALLOCATABLE::t4B(:,:,:,:,:,:,:,:)
!       REAL*8 t4B(N2+1:N3,N1+1:N3,N1+1:N3,N1+1:N3,
!     & N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N1)
       REAL*8,ALLOCATABLE::t4C(:,:,:,:,:,:,:,:)
!       REAL*8 t4C(N2+1:N3,N2+1:N3,N1+1:N3,N1+1:N3,
!     & N0+1:N2,N0+1:N2,N0+1:N1,N0+1:N1)
!       REAL*8,ALLOCATABLE::t4D(:,:,:,:,:,:,:,:)
!       REAL*8 t4D(N2+1:N3,N2+1:N3,N2+1:N3,N1+1:N3,
!     & N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N1)
!       REAL*8,ALLOCATABLE::t4E(:,:,:,:,:,:,:,:)
!       REAL*8 t4E(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:N3,
!     & N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2)
       REAL*8,ALLOCATABLE::V4B(:,:,:,:,:,:,:,:)
!       REAL*8 V4B(N2+1:N3,N1+1:N3,N1+1:N3,N1+1:N3,
!     & N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N1)
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
       ALLOCATE(t4B(N2+1:N3,N1+1:N3,N1+1:N3,N1+1:N3,
     & N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N1))
C
       REWIND(tb)
       READ(tb)t4b
C       
C
       DO i=N0+1,N1-2
       DO j=i+1,N1-1
       DO k=j+1,N1
       DO l=N0+1,N2
       DO a=N1+1,N3-2
       DO b=a+1,N3-1
       DO c=b+1,N3
       DO d=N2+1,N3
!
         t4B(d,c,b,a,l,k,i,j)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,c,b,a,l,i,j,k)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,c,b,a,l,i,k,j)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,c,b,a,l,j,k,i)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,c,b,a,l,j,i,k)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,c,a,b,l,k,j,i)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,c,a,b,l,k,i,j)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,c,a,b,l,i,j,k)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,c,a,b,l,i,k,j)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,c,a,b,l,j,k,i)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,c,a,b,l,j,i,k)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,a,b,c,l,k,j,i)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,a,b,c,l,k,i,j)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,a,b,c,l,i,j,k)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,a,b,c,l,i,k,j)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,a,b,c,l,j,k,i)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,a,b,c,l,j,i,k)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,a,c,b,l,k,j,i)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,a,c,b,l,k,i,j)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,a,c,b,l,i,j,k)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,a,c,b,l,i,k,j)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,a,c,b,l,j,k,i)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,a,c,b,l,j,i,k)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,b,c,a,l,k,j,i)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,b,c,a,l,k,i,j)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,b,c,a,l,i,j,k)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,b,c,a,l,i,k,j)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,b,c,a,l,j,k,i)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,b,c,a,l,j,i,k)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,b,a,c,l,k,j,i)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,b,a,c,l,k,i,j)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,b,a,c,l,i,j,k)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,b,a,c,l,i,k,j)= t4B(d,c,b,a,l,k,j,i)
         t4B(d,b,a,c,l,j,k,i)=-t4B(d,c,b,a,l,k,j,i)
         t4B(d,b,a,c,l,j,i,k)= t4B(d,c,b,a,l,k,j,i)
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       REWIND(tb)
       WRITE(tb)t4b
	CALL CPU_TIME(timt2)
C
       end
