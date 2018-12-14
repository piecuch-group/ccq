       subroutine tran3Bto3C(N0,N1,N2,N3,t3B,t3C)
C
       integer a,b,c
       REAL*8 t3B(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1)
       REAL*8 t3C(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1)
C
       DO i=N0+1,N1
       DO j=N0+1,N2-1
       DO k=j+1,N2
       DO a=N1+1,N3
       DO b=N2+1,N3-1
       DO c=b+1,N3
         t3C(c,b,a,k,j,i)= t3B(a,c,b,i,k,j)
         t3C(c,b,a,j,k,i)=-t3C(c,b,a,k,j,i)
         t3C(b,c,a,k,j,i)=-t3C(c,b,a,k,j,i)
         t3C(b,c,a,j,k,i)= t3C(c,b,a,k,j,i)
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
       end
