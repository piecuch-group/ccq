       SUBROUTINE t3D_antisym(N0,N1,N2,N3,K1,K2,K3,K4,lvl1,lvlq,shift,V3D
     & ,FockR,FockB,IntR,IntB,IntM,t1A,t1B,t2A,t2B,t2C,t3A,t3B,t3C,t3D,
     & iactoccb,iactunob,iactindt,
     & t2diag3,t2diag4,t2diag5,t3diag1,t3diag2,t3diag3,t3diag4,t3diag5)
!     & t4A,t4B,t4C,t4D,t4E)
C
       INTEGER a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p
       INTEGER iactoccb,iactunob,iactindt
       INTEGER ioccb,iunob
       INTEGER,ALLOCATABLE::inDOcc(:,:,:)
       INTEGER,ALLOCATABLE::indunocc(:,:,:)
       CHARACTER lvl1*6,lvlq*6
!       INTEGER inDOcc(N0+1:N2,N0+1:N2,N0+1:N2)
!       INTEGER indunocc(N2+1:N3,N2+1:N3,N2+1:N3)
       REAL t2diag3,t2diag4,t2diag5
       REAL t3diag1,t3diag2,t3diag3,t3diag4,t3diag5
       REAL factor
       REAL*8 shift,PP,Coeleft,time1,time2,timt1,timt2
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
C
       DO i=N0+1,N2-2
       DO j=i+1,N2-1
       DO k=j+1,N2
       DO a=N2+1,N3-2
       DO b=a+1,N3-1
       DO c=b+1,N3
!
         t3D(c,b,a,k,i,j)=-t3D(c,b,a,k,j,i)
         t3D(c,b,a,i,j,k)=-t3D(c,b,a,k,j,i)
         t3D(c,b,a,i,k,j)= t3D(c,b,a,k,j,i)
         t3D(c,b,a,j,k,i)=-t3D(c,b,a,k,j,i)
         t3D(c,b,a,j,i,k)= t3D(c,b,a,k,j,i)
         t3D(c,a,b,k,j,i)=-t3D(c,b,a,k,j,i)
         t3D(c,a,b,k,i,j)= t3D(c,b,a,k,j,i)
         t3D(c,a,b,i,j,k)= t3D(c,b,a,k,j,i)
         t3D(c,a,b,i,k,j)=-t3D(c,b,a,k,j,i)
         t3D(c,a,b,j,k,i)= t3D(c,b,a,k,j,i)
         t3D(c,a,b,j,i,k)=-t3D(c,b,a,k,j,i)
         t3D(a,b,c,k,j,i)=-t3D(c,b,a,k,j,i)
         t3D(a,b,c,k,i,j)= t3D(c,b,a,k,j,i)
         t3D(a,b,c,i,j,k)= t3D(c,b,a,k,j,i)
         t3D(a,b,c,i,k,j)=-t3D(c,b,a,k,j,i)
         t3D(a,b,c,j,k,i)= t3D(c,b,a,k,j,i)
         t3D(a,b,c,j,i,k)=-t3D(c,b,a,k,j,i)
         t3D(a,c,b,k,j,i)= t3D(c,b,a,k,j,i)
         t3D(a,c,b,k,i,j)=-t3D(c,b,a,k,j,i)
         t3D(a,c,b,i,j,k)=-t3D(c,b,a,k,j,i)
         t3D(a,c,b,i,k,j)= t3D(c,b,a,k,j,i)
         t3D(a,c,b,j,k,i)=-t3D(c,b,a,k,j,i)
         t3D(a,c,b,j,i,k)= t3D(c,b,a,k,j,i)
         t3D(b,c,a,k,j,i)=-t3D(c,b,a,k,j,i)
         t3D(b,c,a,k,i,j)= t3D(c,b,a,k,j,i)
         t3D(b,c,a,i,j,k)= t3D(c,b,a,k,j,i)
         t3D(b,c,a,i,k,j)=-t3D(c,b,a,k,j,i)
         t3D(b,c,a,j,k,i)= t3D(c,b,a,k,j,i)
         t3D(b,c,a,j,i,k)=-t3D(c,b,a,k,j,i)
         t3D(b,a,c,k,j,i)= t3D(c,b,a,k,j,i)
         t3D(b,a,c,k,i,j)=-t3D(c,b,a,k,j,i)
         t3D(b,a,c,i,j,k)=-t3D(c,b,a,k,j,i)
         t3D(b,a,c,i,k,j)= t3D(c,b,a,k,j,i)
         t3D(b,a,c,j,k,i)=-t3D(c,b,a,k,j,i)
         t3D(b,a,c,j,i,k)= t3D(c,b,a,k,j,i)
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
	CALL CPU_TIME(time2)
C
       end
