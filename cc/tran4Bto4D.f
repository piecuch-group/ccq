       subroutine tran4Bto4D(N0,N1,N2,N3)
C
       integer a,b,c,d,i,j,k,l
       real*8 timt1,timt2
       real*8,allocatable::t4B(:,:,:,:,:,:,:,:)
       real*8,allocatable::t4D(:,:,:,:,:,:,:,:)
C
       INTEGER ta,tb,tc,td,te
       parameter(ta=29,tb=30,tc=31,td=32,te=33)
C
       allocate(t4B(N2+1:N3,N1+1:N3,N1+1:N3,N1+1:N3,
     & N0+1:N2,N0+1:N1,N0+1:N1,N0+1:N1))
       allocate(t4D(N2+1:N3,N2+1:N3,N2+1:N3,N1+1:N3,
     & N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N1))
C
       rewind(tb)
       rewind(td)
       READ(tb)t4B
       READ(td)t4D
C
C
       do i=N0+1,N1
       do j=N0+1,N2-2
       do k=j+1,N2-1
       do l=k+1,N2
       do a=N1+1,N3
       do b=N2+1,N3-2
       do c=b+1,N3-1
       do d=c+1,N3

         t4D(d,c,b,a,l,k,j,i)= t4B(a,d,c,b,i,l,k,j)
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
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       rewind(td)
       WRITE(td)t4D
	call CPU_TIME(timt2)
C
       end
