      subroutine old_energy(n0,n1,n2,n3,
     & fockr,fockb,intr,intb,intm,t1a,t1b,t2a,t2b,t2c,
     & e1a,e1b,e2a,e2b,e2c,e1a1a,e1a1b,e1b1b)

        real*8 eqnright,pp
        integer a,b,c,d,e,f,i,j,k,l,m,n,r,s,t,u
        real*8 e1a,e1b,e2a,e2b,e2c,e1a1a,e1b1b,e1a1b

        real*8 intr(n0+1:n3,n0+1:n3,n0+1:n3,n0+1:n3)
        real*8 intb(n0+1:n3,n0+1:n3,n0+1:n3,n0+1:n3)
        real*8 intm(n0+1:n3,n0+1:n3,n0+1:n3,n0+1:n3)
        real*8 fockr(n3,n3)
        real*8 fockb(n3,n3)
        real*8 t1a(n1+1:n3,n0+1:n1)
        real*8 t1b(n2+1:n3,n0+1:n2)
        real*8 t2a(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1)
        real*8 t2b(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1)
        real*8 t2c(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2)

        e2a=0.0d0
        e2b=0.0d0
        e2c=0.0d0
        do m=n0+1,n1
          do n=n0+1,n1
            do e=n1+1,n3
              do f=n1+1,n3
                e2a=e2a+0.25*intr(e,f,m,n)*t2a(f,e,n,m)
              enddo
            enddo
          enddo
        enddo

        do m=n0+1,n2
          do n=n0+1,n2
            do e=n2+1,n3
              do f=n2+1,n3
                e2c=e2c+0.25*intb(e,f,m,n)*t2c(f,e,n,m)
              enddo
            enddo
          enddo
        enddo

        do m=n0+1,n1
          do n=n0+1,n2
            do e=n1+1,n3
              do f=n2+1,n3
                e2b=e2b+intm(e,f,m,n)*t2b(f,e,n,m)
              enddo
            enddo
          enddo
        enddo

        e1a=0.0d0
        e1b=0.0d0
        do e= n1+1,n3
          do m= n0+1,n1
            e1a=e1a+fockr(m,e)*t1a(e,m)
          enddo
        enddo
        do e= n2+1,n3
          do m= n0+1,n2
            e1b=e1b+fockb(m,e)*t1b(e,m)
          enddo
        enddo

        e1a1a=0.0d0
        e1a1b=0.0d0
        e1b1b=0.0d0
        do m=n0+1,n1
          do n=n0+1,n1
            do e=n1+1,n3
              do f=n1+1,n3
                e1a1a=e1a1a+0.50*intr(e,f,m,n)*t1a(f,n)*t1a(e,m)
              enddo
            enddo
          enddo
        enddo
        do m=n0+1,n2
          do n=n0+1,n2
            do e=n2+1,n3
              do f=n2+1,n3
                e1b1b=e1b1b+0.50*intb(e,f,m,n)*t1b(f,n)*t1b(e,m)
              enddo
            enddo
          enddo
        enddo
        do m=n0+1,n1
          do n=n0+1,n2
            do e=n1+1,n3
              do f=n2+1,n3
                e1a1b=e1a1b+intm(e,f,m,n)*t1a(e,m)*t1b(f,n)
              enddo
            enddo
          enddo
        enddo

        end
