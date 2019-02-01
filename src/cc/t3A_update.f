       subroutine t3a_update(n0,n1,n2,n3,k1,k2,k3,k4,lvl_q,shift,v3a
     & ,fockr,fockb,intr,intb,intm,t1a,t1b,t2a,t2b,t2c,t3a,t3b,t3c,t3d,
     & iactocca,iactunoa,iactindt,
     & t2diag3,t2diag4,t2diag5,t3diag1,t3diag2,t3diag3,t3diag4,t3diag5)
!     & t4a,t4b,t4c,t4d,t4e)
c
       integer a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p
       integer iactocca,iactunoa,iactindt
       integer iocca,iunoa
       real t2diag3,t2diag4,t2diag5
       real t3diag1,t3diag2,t3diag3,t3diag4,t3diag5
       real factor
       real*8 shift,pp,coeleft,time1,time2
       integer,allocatable::indocc(:,:,:)
       integer,allocatable::indunocc(:,:,:)
       logical lvl_q
!       integer indocc(n0+1:n1,n0+1:n1,n0+1:n1)
!       integer indunocc(n1+1:n3,n1+1:n3,n1+1:n3)
       real*8 fockr(n3,n3)
       real*8 fockb(n3,n3)
       real*8 sum
       real*8 intr(n0+1:n3,n0+1:n3,n0+1:n3,n0+1:n3)
       real*8 intb(n0+1:n3,n0+1:n3,n0+1:n3,n0+1:n3)
       real*8 intm(n0+1:n3,n0+1:n3,n0+1:n3,n0+1:n3)
       real*8 t1a(n1+1:n3,n0+1:n1)
       real*8 t1b(n2+1:n3,n0+1:n2)
       real*8 t2a(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1)
       real*8 t2b(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1)
       real*8 t2c(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2)
       real*8 t3a(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1)
       real*8 t3b(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1)
       real*8 t3c(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1)
       real*8 t3d(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2)
!       real*8 t4a(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1)
!!!!!       real*8 t4a(k3*k3*k3*k3,k1*k1*k1*k1)
!       real*8 t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1)
!!!!!       real*8 t4b(k4*k3*k3*k3,k2*k1*k1*k1)
!       real*8 t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1)
!!!!!       real*8 t4c(k4*k4*k3*k3,k2*k2*k1*k1)
!       real*8 t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1)
!!!!!       real*8 t4d(k4*k4*k4*k3,k2*k2*k2*k1)
!       real*8 t4e(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2)
!!!!!       real*8 t4e(k4*k4*k4*k4,k2*k2*k2*k2)
       real*8 v3a(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1)
c
       real*8,allocatable::t4a(:,:,:,:,:,:,:,:)                     !ilias: if no quadruples comment out the following 2 lines
       real*8,allocatable::t4b(:,:,:,:,:,:,:,:)
!       real*8,allocatable::t4c(:,:)
!       real*8,allocatable::t4d(:,:)
!       real*8,allocatable::t4e(:,:)
c
       integer ta,tb,tc,td,te                                       !ilias: if no quadruples comment out the following 2 lines
       parameter(ta=29,tb=30,tc=31,td=32,te=33)
c
       integer i1,i2
       integer,allocatable::ind1(:,:,:,:)
       integer,allocatable::ind2(:,:,:,:)
c
       real*8,allocatable::b1(:,:)
       real*8,allocatable::b2(:,:)
       real*8,allocatable::d1(:,:,:,:)
       real*8,allocatable::d2(:,:,:,:)
       real*8,allocatable::f2(:,:,:,:,:,:)
       real*8,allocatable::h2(:,:,:,:,:,:,:,:)
c
       real*8,allocatable::s1(:,:,:,:)
       real*8,allocatable::s2(:,:,:,:)
       real*8,allocatable::s3(:,:,:,:)
       real*8,allocatable::s4(:,:,:,:)
       real*8,allocatable::q1(:,:)
       real*8,allocatable::q2(:,:)
       real*8,allocatable::s5(:,:,:,:)
       real*8,allocatable::s32(:,:,:,:)
       real*8,allocatable::s6(:,:,:,:)
       real*8,allocatable::s33(:,:,:,:)
       real*8,allocatable::q3(:,:)
       real*8,allocatable::s7(:,:,:,:)
       real*8,allocatable::s35(:,:,:,:)
       real*8,allocatable::s34(:,:,:,:)
       real*8,allocatable::s8(:,:,:,:)
       real*8,allocatable::q4(:,:)
       real*8,allocatable::s9(:,:,:,:)
       real*8,allocatable::s10(:,:,:,:)
       real*8,allocatable::s11(:,:,:,:)
       real*8,allocatable::s42(:,:,:,:)
       real*8,allocatable::s41(:,:,:,:)
       real*8,allocatable::s40(:,:,:,:)
       real*8,allocatable::q15(:,:)
       real*8,allocatable::s37(:,:,:,:)
       real*8,allocatable::s51(:,:,:,:)
       real*8,allocatable::s36(:,:,:,:)
       real*8,allocatable::s50(:,:,:,:)
       real*8,allocatable::s12(:,:,:,:)
       real*8,allocatable::s45(:,:,:,:)
       real*8,allocatable::s44(:,:,:,:)
       real*8,allocatable::s43(:,:,:,:)
       real*8,allocatable::q16(:,:)
       real*8,allocatable::s38(:,:,:,:)
       real*8,allocatable::q5(:,:)
       real*8,allocatable::s46(:,:,:,:)
       real*8,allocatable::s13(:,:,:,:)
       real*8,allocatable::s47(:,:,:,:)
       real*8,allocatable::q17(:,:)
       real*8,allocatable::s39(:,:,:,:)
       real*8,allocatable::s14(:,:,:,:)
       real*8,allocatable::s48(:,:,:,:)
       real*8,allocatable::q18(:,:)
       real*8,allocatable::q6(:,:)
       real*8,allocatable::q7(:,:)
       real*8,allocatable::q8(:,:)
       real*8,allocatable::q9(:,:)
       real*8,allocatable::s49(:,:,:,:)
       real*8,allocatable::q10(:,:)
       real*8,allocatable::s15(:,:,:,:)
       real*8,allocatable::s16(:,:,:,:)
       real*8,allocatable::s17(:,:,:,:)
       real*8,allocatable::s18(:,:,:,:)
       real*8,allocatable::s19(:,:,:,:)
       real*8,allocatable::s20(:,:,:,:)
       real*8,allocatable::s21(:,:,:,:)
       real*8,allocatable::s22(:,:,:,:)
       real*8,allocatable::s23(:,:,:,:)
       real*8,allocatable::s24(:,:,:,:)
       real*8,allocatable::s25(:,:,:,:)
       real*8,allocatable::q11(:,:)
       real*8,allocatable::s26(:,:,:,:)
       real*8,allocatable::q12(:,:)
       real*8,allocatable::s27(:,:,:,:)
       real*8,allocatable::s28(:,:,:,:)
       real*8,allocatable::s29(:,:,:,:)
       real*8,allocatable::s30(:,:,:,:)
       real*8,allocatable::q13(:,:)
       real*8,allocatable::q14(:,:)
       real*8,allocatable::s31(:,:,:,:)
       real*8,allocatable::x1(:,:,:,:)
       real*8,allocatable::z1(:,:,:,:,:,:)
       real*8,allocatable::x2(:,:,:,:)
       real*8,allocatable::z2(:,:,:,:,:,:)
       real*8,allocatable::x3(:,:)
       real*8,allocatable::z3(:,:,:,:,:,:)
       real*8,allocatable::x4(:,:)
       real*8,allocatable::z4(:,:,:,:,:,:)
       real*8,allocatable::x5(:,:,:,:)
       real*8,allocatable::z5(:,:,:,:,:,:)
       real*8,allocatable::x6(:,:,:,:)
       real*8,allocatable::z6(:,:,:,:,:,:)
       real*8,allocatable::x7(:,:,:,:)
       real*8,allocatable::z7(:,:,:,:,:,:)
       real*8,allocatable::x8(:,:,:,:)
       real*8,allocatable::z8(:,:,:,:,:,:)
       real*8,allocatable::x9(:,:)
       real*8,allocatable::z9(:,:,:,:,:,:)
       real*8,allocatable::x10(:,:,:,:)
       real*8,allocatable::z10(:,:,:,:,:,:)
       real*8,allocatable::x11(:,:,:,:)
       real*8,allocatable::z11(:,:,:,:,:,:)
       real*8,allocatable::x12(:,:)
       real*8,allocatable::z12(:,:,:,:,:,:)
       real*8,allocatable::x13(:,:,:,:)
       real*8,allocatable::z13(:,:,:,:,:,:)
       real*8,allocatable::x14(:,:,:,:)
       real*8,allocatable::z14(:,:,:,:,:,:)
       real*8,allocatable::x15(:,:,:,:)
       real*8,allocatable::z16(:,:,:,:,:,:)
       real*8,allocatable::x16(:,:,:,:)
       real*8,allocatable::z17(:,:,:,:,:,:)
       real*8,allocatable::z21(:,:,:,:,:,:)
       real*8,allocatable::z25(:,:,:,:,:,:)
       real*8,allocatable::z74(:,:,:,:,:,:)
       real*8,allocatable::z73(:,:,:,:,:,:)
       real*8,allocatable::z72(:,:,:,:,:,:)
       real*8,allocatable::z76(:,:,:,:,:,:)
       real*8,allocatable::z75(:,:,:,:,:,:)
       real*8,allocatable::x17(:,:,:,:)
       real*8,allocatable::z79(:,:,:,:,:,:)
       real*8,allocatable::x18(:,:,:,:)
       real*8,allocatable::z80(:,:,:,:,:,:)
       real*8,allocatable::z40(:,:,:,:,:,:)
       real*8,allocatable::z41(:,:,:,:,:,:)
       real*8,allocatable::z42(:,:,:,:,:,:)
       real*8,allocatable::x19(:,:,:,:)
       real*8,allocatable::z46(:,:,:,:,:,:)
       real*8,allocatable::x20(:,:,:,:)
       real*8,allocatable::z48(:,:,:,:,:,:)
c
       allocate(indocc(n0+1:n1,n0+1:n1,n0+1:n1))
       allocate(indunocc(n1+1:n3,n1+1:n3,n1+1:n3))
       indocc=0
       indunocc=0
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        iocca=0
        if(i.gt.(n1-iactocca))iocca=iocca+1
        if(j.gt.(n1-iactocca))iocca=iocca+1
        if(k.gt.(n1-iactocca))iocca=iocca+1
        if(iocca.lt.iactindt)indocc(k,j,i)=1
       enddo;enddo;enddo
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          iunoa=0
          if(a.lt.(n1+iactunoa+1))iunoa=iunoa+1
          if(b.lt.(n1+iactunoa+1))iunoa=iunoa+1
          if(c.lt.(n1+iactunoa+1))iunoa=iunoa+1
          if(iunoa.lt.iactindt)indunocc(c,b,a)=1
          enddo;enddo;enddo
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s1)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       x1=0.0d0
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x1,s1,-1.000)
       deallocate(s1)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s2(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s2)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x15(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       x15=0.0d0
       call sum3124(n0,n1,n1,n3,n0,n1,n0,n1,x15,s2, 1.000)
       deallocate(s2)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s3(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s3)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x16(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       x16=0.0d0
       call sum3124(n1,n3,n1,n3,n1,n3,n0,n1,x16,s3, 1.000)
       deallocate(s3)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s4(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s4)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x2(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       x2=0.0d0
       call sum4123(n1,n3,n1,n3,n1,n3,n0,n1,x2,s4, 1.000)
       deallocate(s4)
c
       allocate(b1(n1+1:n3,n0+1:n1))
       call reorder12(0,n3,0,n3,
     & n1,n3,n0,n1,fockr,b1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(q1(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,b1,b2,q1)
       deallocate(b1)
       deallocate(b2)
c
       allocate(x3(n0+1:n1,n0+1:n1))
       x3=0.0d0
       call sum21(n0,n1,n0,n1,x3,q1, 1.000)
       deallocate(q1)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n1,n1,n3,fockr,b1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q2(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,b1,b2,q2)
       deallocate(b1)
       deallocate(b2)
c
       allocate(x4(n1+1:n3,n1+1:n3))
       x4=0.0d0
       call sum21(n1,n3,n1,n3,x4,q2,-1.000)
       deallocate(q2)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s5(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s5)
       deallocate(d1)
       deallocate(b2)
c
!       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder2314(n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n0,n1,n0,n1,s5,d1)
!       allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder451236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,t3a,f2)
!       allocate(z21(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1
!       i2=k1*k3*k3*k3
!       i3=k1*k1
!       call egemm(i1,i2,i3,d1,f2,z21)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum
     &     + (s5(j,n,m,i)*t3a(c,b,a,n,m,k)      !kji
     &     - s5(k,n,m,i)*t3a(c,b,a,n,m,j)       !jki(-0.500)
     &     - s5(i,n,m,j)*t3a(c,b,a,n,m,k)       !kij(-0.500)
     &     + s5(i,n,m,k)*t3a(c,b,a,n,m,j)       !jik( 0.500)
     &     + s5(k,n,m,j)*t3a(c,b,a,n,m,i)       !ikj( 0.500)
     &     - s5(j,n,m,k)*t3a(c,b,a,n,m,i))/2.0d0!ijk(-0.500)
             enddo;enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       v3a=v3a+0.500*z21
!       call
!     & sum123546(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z21,-0.500)
!       call
!     & sum123465(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z21,-0.500)
!       call
!     & sum123564(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z21, 0.500)
!       call
!     & sum123645(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z21, 0.500)
!       call
!     & sum123654(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z21,-0.500)
!       deallocate(z21)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder3214(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s5,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s32(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s32)
       deallocate(d1)
       deallocate(b2)
       deallocate(s5)
c
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x15,s32,-1.000)
       deallocate(s32)
c
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z16(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x15,d2,z16)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x15(m,c,j,i)*t2a(b,a,m,k) !bakcji( 1.000)
     &     - x15(m,b,j,i)*t2a(c,a,m,k) !cakbji(-1.000)
     &     + x15(m,a,j,i)*t2a(c,b,m,k) !cbkaji( 1.000)
     &     - x15(m,c,k,i)*t2a(b,a,m,j) !bajcki(-1.000)
     &     + x15(m,b,k,i)*t2a(c,a,m,j) !cajbki( 1.000)
     &     - x15(m,a,k,i)*t2a(c,b,m,j) !cbjaki(-1.000)
     &     - x15(m,c,i,j)*t2a(b,a,m,k) !bakcij(-1.000)
     &     + x15(m,b,i,j)*t2a(c,a,m,k) !cakbij( 1.000)
     &     - x15(m,a,i,j)*t2a(c,b,m,k) !cbkaij(-1.000)
     &     + x15(m,c,i,k)*t2a(b,a,m,j) !bajcik( 1.000)
     &     - x15(m,b,i,k)*t2a(c,a,m,j) !cajbik(-1.000)
     &     + x15(m,a,i,k)*t2a(c,b,m,j) !cbjaik( 1.000)
     &     + x15(m,c,k,j)*t2a(b,a,m,i) !baickj( 1.000)
     &     - x15(m,b,k,j)*t2a(c,a,m,i) !caibkj(-1.000)
     &     + x15(m,a,k,j)*t2a(c,b,m,i) !cbiakj( 1.000)
     &     - x15(m,c,j,k)*t2a(b,a,m,i) !baicjk(-1.000)
     &     + x15(m,b,j,k)*t2a(c,a,m,i) !caibjk( 1.000)
     &     - x15(m,a,j,k)*t2a(c,b,m,i) !cbiajk(-1.000)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum234156(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16, 1.000)
!       call
!     & sum134256(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16,-1.000)
!       call
!     & sum124356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16, 1.000)
!       call
!     & sum235146(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16,-1.000)
!       call
!     & sum135246(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16, 1.000)
!       call
!     & sum125346(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16,-1.000)
!       call
!     & sum234165(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16,-1.000)
!       call
!     & sum134265(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16, 1.000)
!       call
!     & sum124365(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16,-1.000)
!       call
!     & sum235164(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16, 1.000)
!       call
!     & sum135264(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16,-1.000)
!       call
!     & sum125364(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16, 1.000)
!       call
!     & sum236145(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16, 1.000)
!       call
!     & sum136245(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16,-1.000)
!       call
!     & sum126345(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16, 1.000)
!       call
!     & sum236154(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16,-1.000)
!       call
!     & sum136254(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16, 1.000)
!       call
!     & sum126354(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z16,-1.000)
!       deallocate(z16)
!       deallocate(x15)
c
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s6(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s6)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x6(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       x6=0.0d0
       call sum3124(n0,n1,n1,n3,n1,n3,n0,n1,x6,s6,-1.000)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder2314(n1,n3,n0,n1,n1,n3,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s6,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s33(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s33)
       deallocate(d1)
       deallocate(b2)
       deallocate(s6)
c
       call sum2134(n1,n3,n1,n3,n1,n3,n0,n1,x2,s33, 1.000)
       deallocate(s33)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1423(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q3(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q3)
       deallocate(d1)
       deallocate(b2)
c
       x3=x3-q3
       deallocate(q3)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s7(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s7)
       deallocate(d1)
       deallocate(b2)
c
       call sum4123(n0,n1,n1,n3,n1,n3,n0,n1,x6,s7, 1.000)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder2341(n0,n1,n0,n1,n1,n3,n1,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,s7,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s35(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s35)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n1,n3,n1,n3,n1,n3,n0,n1,x16,s35, 1.000)
       deallocate(s35)
c
!       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
!       allocate(z17(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k3
!       i3=k3
!       call egemm(i1,i2,i3,x16,d2,z17)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     - x16(e,c,b,i)*t2a(e,a,k,j)     !ecbieakj (-1.000)
     &     + x16(e,c,a,i)*t2a(e,b,k,j)     !ecaiebkj (+1.000)
     &     + x16(e,b,c,i)*t2a(e,a,k,j)     !ebcieakj (+1.000)
     &     - x16(e,a,c,i)*t2a(e,b,k,j)     !eaciebkj (-1.000)
     &     - x16(e,b,a,i)*t2a(e,c,k,j)     !ebaieckj (-1.000)
     &     + x16(e,a,b,i)*t2a(e,c,k,j)     !eabieckj (+1.000)
     &     + x16(e,c,b,j)*t2a(e,a,k,i)     !ecbjeaki (+1.000)
     &     - x16(e,c,a,j)*t2a(e,b,k,i)     !ecajebki (-1.000)
     &     - x16(e,b,c,j)*t2a(e,a,k,i)     !ebcjeaki (-1.000)
     &     + x16(e,a,c,j)*t2a(e,b,k,i)     !eacjebki (+1.000)
     &     + x16(e,b,a,j)*t2a(e,c,k,i)     !ebajecki (+1.000)
     &     - x16(e,a,b,j)*t2a(e,c,k,i)     !eabjecki (-1.000)
     &     - x16(e,c,b,k)*t2a(e,a,j,i)     !ecbkeaji (-1.000)
     &     + x16(e,c,a,k)*t2a(e,b,j,i)     !ecakebji (+1.000)
     &     + x16(e,b,c,k)*t2a(e,a,j,i)     !ebckeaji (+1.000)
     &     - x16(e,a,c,k)*t2a(e,b,j,i)     !eackebji (-1.000)
     &     - x16(e,b,a,k)*t2a(e,c,j,i)     !ebakecji (-1.000)
     &     + x16(e,a,b,k)*t2a(e,c,j,i)     !eabkecji (+1.000)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum345126(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17,-1.000)
!       call
!     & sum245136(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17, 1.000)
!       call
!     & sum345216(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17, 1.000)
!       call
!     & sum245316(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17,-1.000)
!       call
!     & sum145236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17,-1.000)
!       call
!     & sum145326(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17, 1.000)
!       call
!     & sum346125(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17, 1.000)
!       call
!     & sum246135(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17,-1.000)
!       call
!     & sum346215(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17,-1.000)
!       call
!     & sum246315(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17, 1.000)
!       call
!     & sum146235(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17, 1.000)
!       call
!     & sum146325(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17,-1.000)
!       call
!     & sum356124(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17,-1.000)
!       call
!     & sum256134(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17, 1.000)
!       call
!     & sum356214(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17, 1.000)
!       call
!     & sum256314(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17,-1.000)
!       call
!     & sum156234(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17,-1.000)
!       call
!     & sum156324(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z17, 1.000)
!       deallocate(z17)
!       deallocate(x16)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3241(n0,n1,n0,n1,n1,n3,n1,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,s7,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s34(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s34)
       deallocate(d1)
       deallocate(b2)
       deallocate(s7)
c
       call sum3124(n0,n1,n1,n3,n0,n1,n0,n1,x1,s34, 1.000)
       deallocate(s34)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s8(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s8)
       deallocate(d1)
       deallocate(b2)
c
!       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
!       call reorder2341(n1,n3,n1,n3,n1,n3,n1,n3,
!     & n1,n3,n1,n3,n1,n3,n1,n3,s8,d1)
!       allocate(f2(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder123456(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t3a,f2)
!       allocate(z25(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
!       i1=k3*k3
!       i2=k1*k1*k1*k3
!       i3=k3*k3
!       call egemm(i1,i2,i3,d1,f2,z25)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do f=n1+1,n3
             sum=sum
     &     + (s8(b,f,e,c)*t3a(f,e,a,k,j,i)      !bfecfeakji (+0.500)
     &     - s8(a,f,e,c)*t3a(f,e,b,k,j,i)       !afecfebkji (-0.500)
     &     - s8(c,f,e,b)*t3a(f,e,a,k,j,i)       !cfebfeakji (-0.500)
     &     + s8(c,f,e,a)*t3a(f,e,b,k,j,i)       !cfeafebkji (+0.500)
     &     + s8(a,f,e,b)*t3a(f,e,c,k,j,i)       !afebfeckji (+0.500)
     &     - s8(b,f,e,a)*t3a(f,e,c,k,j,i))/2.0d0!bfeafeckji (-0.500)
             enddo;enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum345612(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z25, 0.500)
!       call
!     & sum245613(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z25,-0.500)
!       call
!     & sum345621(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z25,-0.500)
!       call
!     & sum245631(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z25, 0.500)
!       call
!     & sum145623(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z25, 0.500)
!       call
!     & sum145632(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z25,-0.500)
!       deallocate(z25)
!       deallocate(s8)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder2341(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q4(n1+1:n3,n1+1:n3))
       i1=k3*k3
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q4)
       deallocate(d1)
       deallocate(b2)
c
       x4=x4-q4
       deallocate(q4)
c
       allocate(d1(n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n2,n3,n0,n1,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s9(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       i1=k1*k4*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s9)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x8(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       x8=0.0d0
       call sum3124(n0,n2,n2,n3,n1,n3,n0,n1,x8,s9,-1.000)
       deallocate(s9)
c
       allocate(d1(n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n2,n3,n1,n3,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s10(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       i1=k3*k4*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s10)
       deallocate(d1)
       deallocate(b2)
c
       call sum4123(n0,n2,n2,n3,n1,n3,n0,n1,x8,s10, 1.000)
       deallocate(s10)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n1,n3,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s11(n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s11)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x10(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       x10=0.0d0
       call sum4123(n0,n1,n0,n1,n1,n3,n0,n1,x10,s11, 1.000)
c
       call sumx2143(n0,n3,n0,n1,n0,n1,n1,n3,n0,n1,x10,intr, 1.000)
c
      if (lvl_q) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4a(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       rewind(ta)
       read(ta)t4a
!       allocate(h2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       call reorder56123478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4a,h2)
!       allocate(z10(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1
!       i2=k1*k1*k3*k3*k3
!       i3=k3*k1*k1
!       call egemm(i1,i2,i3,x10,h2,z10)
!       deallocate(h2)
!       deallocate(t4a)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1                     !ilias: if no quadruples comment out the following 14 lines
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1;do n=n0+1,n1
             sum=sum                                 !top 2 switched
     &     + (x10(n,m,e,j)*t4a(e,c,b,a,n,m,k,i)      !jcbaki(+0.500)
     &     - x10(n,m,e,i)*t4a(e,c,b,a,n,m,k,j)       !icbakj(-0.500)
     &     - x10(n,m,e,k)*t4a(e,c,b,a,n,m,j,i))/2.0d0!kcbaji(-0.500)
             enddo;enddo;enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       v3a=v3a-0.500*z10
!       call
!     & sum123465(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z10, 0.500)
!       call
!     & sum123564(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z10,-0.500)
!       deallocate(z10)
       deallocate(x10)                                              !ilias: if no quadruples comment out the following 2 lines
       deallocate(t4a)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      endif
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,s11,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s42(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s42)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder3124(n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n0,n1,n0,n1,s42,d1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z74(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,d1,d2,z74)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - s42(b,k,m,i)*t2a(c,a,m,j)     !bkicaj (-1.000)
     &     - s42(c,j,m,i)*t2a(b,a,m,k)     !cjibak (-1.000)
     &     + s42(c,k,m,i)*t2a(b,a,m,j)     !ckibaj (+1.000)
     &     + s42(b,j,m,i)*t2a(c,a,m,k)     !bjicak (+1.000)
     &     + s42(b,k,m,j)*t2a(c,a,m,i)     !bkjcai (+1.000)
     &     + s42(c,i,m,j)*t2a(b,a,m,k)     !cijbak (+1.000)
     &     - s42(c,k,m,j)*t2a(b,a,m,i)     !ckjbai (-1.000)
     &     - s42(b,i,m,j)*t2a(c,a,m,k)     !bijcak (-1.000)
     &     - s42(b,j,m,k)*t2a(c,a,m,i)     !bjkcai (-1.000)
     &     - s42(c,i,m,k)*t2a(b,a,m,j)     !cikbaj (-1.000)
     &     + s42(c,j,m,k)*t2a(b,a,m,i)     !cjkbai (+1.000)
     &     + s42(b,i,m,k)*t2a(c,a,m,j)     !bikcaj (+1.000)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum135246(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z74,-1.000)
!       call
!     & sum234156(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z74,-1.000)
!       call
!     & sum235146(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z74, 1.000)
!       call
!     & sum134256(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z74, 1.000)
!       call
!     & sum136245(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z74, 1.000)
!       call
!     & sum234165(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z74, 1.000)
!       call
!     & sum236145(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z74,-1.000)
!       call
!     & sum134265(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z74,-1.000)
!       call
!     & sum136254(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z74,-1.000)
!       call
!     & sum235164(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z74,-1.000)
!       call
!     & sum236154(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z74, 1.000)
!       call
!     & sum135264(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z74, 1.000)
!       deallocate(z74)
       deallocate(s42)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3421(n0,n1,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,s11,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s41(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s41)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder3124(n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n0,n1,n0,n1,s41,d1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z73(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,d1,d2,z73)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     - s41(a,k,n,i)*t2a(c,b,n,j)     !akicbj (-1.000)
     &     + s41(a,j,n,i)*t2a(c,b,n,k)     !ajicbk (+1.000)
     &     + s41(a,k,n,j)*t2a(c,b,n,i)     !akjcbi (+1.000)
     &     - s41(a,i,n,j)*t2a(c,b,n,k)     !aijcbk (-1.000)
     &     - s41(a,j,n,k)*t2a(c,b,n,i)     !ajkcbi (-1.000)
     &     + s41(a,i,n,k)*t2a(c,b,n,j)     !aikcbj (+1.000)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum125346(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z73,-1.000)
!       call
!     & sum124356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z73, 1.000)
!       call
!     & sum126345(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z73, 1.000)
!       call
!     & sum124365(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z73,-1.000)
!       call
!     & sum126354(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z73,-1.000)
!       call
!     & sum125364(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z73, 1.000)
!       deallocate(z73)
       deallocate(s41)
c
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder2341(n0,n1,n0,n1,n0,n1,n1,n3,
     & n0,n1,n0,n1,n1,n3,n0,n1,s11,d1)
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3412(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(s40(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k3*k3
       i3=k1*k1
       call egemm(i1,i2,i3,d1,d2,s40)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n1,n3,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n0,n1,s40,d1)
!       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
!       allocate(z72(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k3
!       i3=k3
!       call egemm(i1,i2,i3,d1,d2,z72)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3
             sum=sum                     !top 2 switched
     &     + (s40(c,a,f,i)*t2a(f,b,k,j)      !caibkj (+0.500)
     &     - s40(b,a,f,i)*t2a(f,c,k,j)       !baickj (-0.500)
     &     - s40(c,b,f,i)*t2a(f,a,k,j)       !cbiakj (-0.500)
     &     + s40(b,a,f,j)*t2a(f,c,k,i)       !bajcki (+0.500)
     &     - s40(c,a,f,j)*t2a(f,b,k,i)       !cajbki (-0.500)
     &     + s40(c,b,f,j)*t2a(f,a,k,i)       !cbjaki (+0.500)
     &     - s40(b,a,f,k)*t2a(f,c,j,i)       !bakcji (-0.500)
     &     + s40(c,a,f,k)*t2a(f,b,j,i)       !cakbji (+0.500)
     &     - s40(c,b,f,k)*t2a(f,a,j,i))/2.0d0!cbkaji (-0.500)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum145236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z72,-0.500)
!       call
!     & sum245136(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z72, 0.500)
!       call
!     & sum345126(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z72,-0.500)
!       call
!     & sum146235(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z72, 0.500)
!       call
!     & sum246135(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z72,-0.500)
!       call
!     & sum346125(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z72, 0.500)
!       call
!     & sum156234(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z72,-0.500)
!       call
!     & sum256134(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z72, 0.500)
!       call
!     & sum356124(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z72,-0.500)
!       deallocate(z72)
       deallocate(s40)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3421(n0,n1,n0,n1,n0,n1,n1,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,s11,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q15(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q15)
       deallocate(d1)
       deallocate(b2)
c
       x3=x3-q15
       deallocate(q15)
c
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3241(n0,n1,n0,n1,n0,n1,n1,n3,
     & n0,n1,n0,n1,n1,n3,n0,n1,s11,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s37(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s37)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n1,n3,n1,n3,n0,n1,x6,s37,-1.000)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder2314(n1,n3,n0,n1,n1,n3,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s37,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s51(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s51)
       deallocate(d1)
       deallocate(b2)
       deallocate(s37)
c
       call sum2134(n1,n3,n1,n3,n1,n3,n0,n1,x2,s51, 1.000)
       deallocate(s51)
c
       call sumx3241(n0,n3,n1,n3,n1,n3,n1,n3,n0,n1,x2,intr, 1.000)
c
!       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
!       allocate(z2(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k3
!       i3=k3
!       call egemm(i1,i2,i3,x2,d2,z2)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum                     
     &     - x2(e,c,b,i)*t2a(e,a,k,j)     !cbiakj (-1.000)
     &     + x2(e,c,a,i)*t2a(e,b,k,j)     !caibkj (+1.000)
     &     - x2(e,b,a,i)*t2a(e,c,k,j)     !baickj (-1.000)
     &     + x2(e,c,b,j)*t2a(e,a,k,i)     !cbjaki (+1.000)
     &     - x2(e,c,a,j)*t2a(e,b,k,i)     !cajbki (-1.000)
     &     + x2(e,b,a,j)*t2a(e,c,k,i)     !bajcki (+1.000)
     &     - x2(e,c,b,k)*t2a(e,a,j,i)     !cbkaji (-1.000)
     &     + x2(e,c,a,k)*t2a(e,b,j,i)     !cakbji (+1.000)
     &     - x2(e,b,a,k)*t2a(e,c,j,i)     !bakcji (-1.000)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum345126(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z2,-1.000)
!       call
!     & sum245136(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z2, 1.000)
!       call
!     & sum145236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z2,-1.000)
!       call
!     & sum346125(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z2, 1.000)
!       call
!     & sum246135(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z2,-1.000)
!       call
!     & sum146235(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z2, 1.000)
!       call
!     & sum356124(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z2,-1.000)
!       call
!     & sum256134(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z2, 1.000)
!       call
!     & sum156234(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z2,-1.000)
!       deallocate(z2)
       deallocate(x2)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4231(n0,n1,n0,n1,n0,n1,n1,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,s11,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s36(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s36)
       deallocate(d1)
       deallocate(b2)
       deallocate(s11)
c
       allocate(x5(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       x5=0.0d0
       call sum3124(n0,n1,n0,n1,n0,n1,n0,n1,x5,s36, 1.000)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder3214(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s36,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s50(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s50)
       deallocate(d1)
       deallocate(b2)
       deallocate(s36)
c
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x1,s50,-1.000)
       deallocate(s50)
c
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s12(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s12)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x11(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       x11=0.0d0
       call sum4123(n0,n1,n1,n3,n1,n3,n1,n3,x11,s12,-1.000)
c
       call sumx4132(n0,n3,n0,n1,n1,n3,n1,n3,n1,n3,x11,intr, 1.000)
c
      if (lvl_q) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4a(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       rewind(ta)
       read(ta)t4a
!       allocate(h2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       call reorder51234678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t4a,h2)
!       allocate(z11(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3))
!       i1=k3
!       i2=k1*k1*k1*k3*k3
!       i3=k3*k3*k1
!       call egemm(i1,i2,i3,x11,h2,z11)
!       deallocate(h2)
!       deallocate(t4a)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1                     !ilias: if no quadruples comment out the following 14 lines
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do f=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + (x11(m,f,e,c)*t4a(f,e,b,a,m,k,j,i)      !cbakji(+0.500)
     &     - x11(m,f,e,b)*t4a(f,e,c,a,m,k,j,i)       !bcakji(-0.500)
     &     + x11(m,f,e,a)*t4a(f,e,c,b,m,k,j,i))/2.0d0!acbkji(+0.500)
             enddo;enddo;enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum234561(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z11, 0.500)
!       call
!     & sum134562(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z11,-0.500)
!       call
!     & sum124563(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z11, 0.500)
!       deallocate(z11)
       deallocate(x11)                                              !ilias: if no quadruples comment out the following 2 lines
       deallocate(t4a)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      endif
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       call reorder3421(n1,n3,n0,n1,n1,n3,n1,n3,
     & n1,n3,n1,n3,n0,n1,n1,n3,s12,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s45(n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k1*k1
       i3=k3*k3
       call egemm(i1,i2,i3,d1,d2,s45)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n0,n1,n1,n3,n0,n1,n0,n1,x1,s45,-0.500)
       deallocate(s45)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder2341(n1,n3,n0,n1,n1,n3,n1,n3,
     & n0,n1,n1,n3,n1,n3,n1,n3,s12,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s44(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s44)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3412(n1,n3,n0,n1,n1,n3,n1,n3,
!     & n1,n3,n1,n3,n1,n3,n0,n1,s44,d1)
!       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
!       allocate(z76(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k3
!       i3=k3
!       call egemm(i1,i2,i3,d1,d2,z76)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + s44(b,k,e,c)*t2a(e,a,j,i)     !bkcaji (+1.000)
     &     - s44(c,k,e,b)*t2a(e,a,j,i)     !ckbaji (-1.000)
     &     + s44(c,k,e,a)*t2a(e,b,j,i)     !ckabji (+1.000)
     &     - s44(b,j,e,c)*t2a(e,a,k,i)     !bjcaki (-1.000)
     &     + s44(c,j,e,b)*t2a(e,a,k,i)     !cjbaki (+1.000)
     &     - s44(c,j,e,a)*t2a(e,b,k,i)     !cjabki (-1.000)
     &     + s44(b,i,e,c)*t2a(e,a,k,j)     !bicakj (+1.000)
     &     - s44(c,i,e,b)*t2a(e,a,k,j)     !cibakj (-1.000)
     &     + s44(c,i,e,a)*t2a(e,b,k,j)     !ciabkj (+1.000)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum356124(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z76, 1.000)
!       call
!     & sum356214(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z76,-1.000)
!       call
!     & sum256314(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z76, 1.000)
!       call
!     & sum346125(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z76,-1.000)
!       call
!     & sum346215(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z76, 1.000)
!       call
!     & sum246315(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z76,-1.000)
!       call
!     & sum345126(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z76, 1.000)
!       call
!     & sum345216(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z76,-1.000)
!       call
!     & sum245316(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z76, 1.000)
!       deallocate(z76)
       deallocate(s44)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder2431(n1,n3,n0,n1,n1,n3,n1,n3,
     & n0,n1,n1,n3,n1,n3,n1,n3,s12,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s43(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s43)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3412(n1,n3,n0,n1,n1,n3,n1,n3,
!     & n1,n3,n1,n3,n1,n3,n0,n1,s43,d1)
!       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
!       allocate(z75(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k3
!       i3=k3
!       call egemm(i1,i2,i3,d1,d2,z75)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3
             sum=sum
     &     + s43(a,k,f,c)*t2a(f,b,j,i)     !akcbji    (+1.000)
     &     - s43(a,k,f,b)*t2a(f,c,j,i)     !akbcji    (-1.000)
     &     + s43(b,k,f,a)*t2a(f,c,j,i)     !bkacji    (+1.000)
     &     - s43(a,j,f,c)*t2a(f,b,k,i)     !ajcbki    (-1.000)
     &     + s43(a,j,f,b)*t2a(f,c,k,i)     !ajbcki    (+1.000)
     &     - s43(b,j,f,a)*t2a(f,c,k,i)     !bjacki    (-1.000)
     &     + s43(a,i,f,c)*t2a(f,b,k,j)     !aicbkj    (+1.000)
     &     - s43(a,i,f,b)*t2a(f,c,k,j)     !aibckj    (-1.000)
     &     + s43(b,i,f,a)*t2a(f,c,k,j)     !biackj    (+1.000)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum256134(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z75, 1.000)
!       call
!     & sum156234(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z75,-1.000)
!       call
!     & sum156324(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z75, 1.000)
!       call
!     & sum246135(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z75,-1.000)
!       call
!     & sum146235(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z75, 1.000)
!       call
!     & sum146325(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z75,-1.000)
!       call
!     & sum245136(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z75, 1.000)
!       call
!     & sum145236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z75,-1.000)
!       call
!     & sum145326(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z75, 1.000)
!       deallocate(z75)
       deallocate(s43)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder2431(n1,n3,n0,n1,n1,n3,n1,n3,
     & n0,n1,n1,n3,n1,n3,n1,n3,s12,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q16(n1+1:n3,n1+1:n3))
       i1=k3*k3
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q16)
       deallocate(d1)
       deallocate(b2)
c
       x4=x4+q16
       deallocate(q16)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder2341(n1,n3,n0,n1,n1,n3,n1,n3,
     & n0,n1,n1,n3,n1,n3,n1,n3,s12,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s38(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s38)
       deallocate(d1)
       deallocate(b2)
       deallocate(s12)
c
       allocate(x7(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       x7=0.0d0
       call sum3124(n1,n3,n1,n3,n1,n3,n1,n3,x7,s38, 1.000)
       deallocate(s38)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q5(n0+1:n1,n1+1:n3))
       i1=k3*k1
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q5)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x9(n0+1:n1,n1+1:n3))
       x9=0.0d0
       x9=x9+q5
c
       allocate(b1(n1+1:n3,n0+1:n1))
       call reorder21(n0,n1,n1,n3,
     & n1,n3,n0,n1,q5,b1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s46(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,b1,d2,s46)
       deallocate(b1)
       deallocate(d2)
       deallocate(q5)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x1,s46, 1.000)
       deallocate(s46)
c
       allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n0,n1,n2,n3,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s13(n0+1:n1,n0+1:n2,n0+1:n1,n2+1:n3))
       i1=k4*k1*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s13)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x13(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       x13=0.0d0
       call sum4123(n0,n2,n0,n1,n2,n3,n0,n1,x13,s13, 1.000)
c
       call sumx2143(n0,n3,n0,n2,n0,n1,n2,n3,n0,n1,x13,intm, 1.000)
c
      if (lvl_q) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       rewind(tb)
       read(tb)t4b
!       allocate(h2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       call reorder56123478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n2,n0,n1,n2,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4b,h2)
!       allocate(z13(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1
!       i2=k1*k1*k3*k3*k3
!       i3=k4*k1*k2
!       call egemm(i1,i2,i3,x13,h2,z13)
!       deallocate(h2)
!       deallocate(t4b)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1                     !ilias: if no quadruples comment out the following 14 lines
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     - x13(n,m,e,i)*t4b(e,c,b,a,n,m,k,j) !icbakj(-1.000)
     &     + x13(n,m,e,j)*t4b(e,c,b,a,n,m,k,i) !jcbaki(+1.000)
     &     - x13(n,m,e,k)*t4b(e,c,b,a,n,m,j,i) !kcbaji(-1.000)
             enddo;enddo;enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       v3a=v3a-z13
!       call
!     & sum123465(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z13, 1.000)
!       call
!     & sum123564(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z13,-1.000)
!       deallocate(z13)
       deallocate(t4b)                                              !ilias: if no quadruples comment out the following 2 lines
       deallocate(x13)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      endif
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n0,n2,n0,n1,n2,n3,
     & n0,n2,n2,n3,n0,n1,n0,n1,s13,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s47(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s47)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x17(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       x17=0.0d0
       call sum2314(n0,n1,n1,n3,n0,n1,n0,n1,x17,s47, 1.000)
       deallocate(s47)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n0,n2,n0,n1,n2,n3,
     & n0,n2,n2,n3,n0,n1,n0,n1,s13,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q17(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q17)
       deallocate(d1)
       deallocate(b2)
c
       x3=x3+q17
       deallocate(q17)
c
       allocate(d1(n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder3241(n0,n1,n0,n2,n0,n1,n2,n3,
     & n0,n1,n0,n2,n2,n3,n0,n1,s13,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s39(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       i1=k1*k4*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s39)
       deallocate(d1)
       deallocate(b2)
       deallocate(s13)
c
       call sum3124(n0,n2,n2,n3,n1,n3,n0,n1,x8,s39,-1.000)
       deallocate(s39)
c
       allocate(d1(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n2,n3,n1,n3,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s14(n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3))
       i1=k3*k4*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s14)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x14(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
       x14=0.0d0
       call sum4123(n0,n2,n2,n3,n1,n3,n1,n3,x14,s14,-1.000)
c
       call sumx4132(n0,n3,n0,n2,n2,n3,n1,n3,n1,n3,x14,intm, 1.000)
c
      if (lvl_q) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       rewind(tb)
       read(tb)t4b
!       allocate(h2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       call reorder51234678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n2,n2,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t4b,h2)
!       allocate(z14(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3))
!       i1=k3
!       i2=k1*k1*k1*k3*k3
!       i3=k3*k4*k2
!       call egemm(i1,i2,i3,x14,h2,z14)
!       deallocate(h2)
!       deallocate(t4b)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1                     !ilias: if no quadruples comment out the following 14 lines
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do f=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x14(m,f,e,c)*t4b(f,e,b,a,m,k,j,i) !cbakji(+1.000)
     &     - x14(m,f,e,b)*t4b(f,e,c,a,m,k,j,i) !bcakji(-1.000)
     &     + x14(m,f,e,a)*t4b(f,e,c,b,m,k,j,i) !acbkji(+1.000)
             enddo;enddo;enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum234561(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z14, 1.000)
!       call
!     & sum134562(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z14,-1.000)
!       call
!     & sum124563(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z14, 1.000)
!       deallocate(z14)
       deallocate(t4b)                                              !ilias: if no quadruples comment out the following 2 lines
       deallocate(x14)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      endif
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
       call reorder2341(n1,n3,n0,n2,n2,n3,n1,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,s14,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s48(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s48)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x18(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       x18=0.0d0
       call sum3412(n1,n3,n1,n3,n1,n3,n0,n1,x18,s48, 1.000)
       deallocate(s48)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
       call reorder2341(n1,n3,n0,n2,n2,n3,n1,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,s14,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q18(n1+1:n3,n1+1:n3))
       i1=k3*k3
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q18)
       deallocate(d1)
       deallocate(b2)
       deallocate(s14)
c
       x4=x4-q18
       deallocate(q18)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n2,n3,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q6(n0+1:n2,n2+1:n3))
       i1=k4*k2
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q6)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x12(n0+1:n2,n2+1:n3))
       x12=0.0d0
       x12=x12+q6
       deallocate(q6)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n0,n1,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q7(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q7)
       deallocate(d1)
       deallocate(b2)
c
       x3=x3+q7
       deallocate(q7)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q8(n1+1:n3,n1+1:n3))
       i1=k3*k3
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q8)
       deallocate(d1)
       deallocate(b2)
c
       x4=x4+q8
       deallocate(q8)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n1+1:n3))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n1,n3,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q9(n0+1:n1,n1+1:n3))
       i1=k3*k1
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q9)
       deallocate(d1)
       deallocate(b2)
c
       x9=x9+q9
c
       call sumx12(0,n3,n0,n1,n1,n3,x9,fockr, 1.000)
c
      if (lvl_q) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4a(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       rewind(ta)
       read(ta)t4a
!       allocate(h2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       call reorder51234678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t4a,h2)
!       allocate(z9(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i2=k1*k1*k1*k3*k3*k3
!       i3=k3*k1
!       call egemm2(i2,i3,x9,h2,z9)
!       deallocate(h2)
!       deallocate(t4a)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1                     !ilias: if no quadruples comment out the following 12 lines
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + x9(m,e)*t4a(e,c,b,a,m,k,j,i) !cbakji(+1.000)
             enddo;enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       v3a=v3a+z9
!       deallocate(z9)
       deallocate(t4a)                                              !ilias: if no quadruples comment out the following 2 lines
       deallocate(x9)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      endif
c
       allocate(b1(n1+1:n3,n0+1:n1))
       call reorder21(n0,n1,n1,n3,
     & n1,n3,n0,n1,q9,b1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s49(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,b1,d2,s49)
       deallocate(b1)
       deallocate(d2)
       deallocate(q9)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x1,s49, 1.000)
       deallocate(s49)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q10(n0+1:n2,n2+1:n3))
       i1=k4*k2
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q10)
       deallocate(d1)
       deallocate(b2)
c
       x12=x12+q10
       deallocate(q10)
c
       call sumx12(0,n3,n0,n2,n2,n3,x12,fockb, 1.000)
c
      if (lvl_q) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       rewind(tb)
       read(tb)t4b
!       allocate(h2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       call reorder51234678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n2,n2,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t4b,h2)
!       allocate(z12(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i2=k1*k1*k1*k3*k3*k3
!       i3=k4*k2
!       call egemm2(i2,i3,x12,h2,z12)
!       deallocate(h2)
!       deallocate(t4b)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1                     !ilias: if no quadruples comment out the following 12 lines
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x12(m,e)*t4b(e,c,b,a,m,k,j,i) !cbakji(+1.000)
             enddo;enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       v3a=v3a+z12
!       deallocate(z12)
       deallocate(x12)                                              !ilias: if no quadruples comment out the following 2 lines
       deallocate(t4b)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      endif
c
       allocate(b1(n1+1:n3,n0+1:n1))
       call reorder12(0,n3,0,n3,
     & n1,n3,n0,n1,fockr,b1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s15(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,b1,d2,s15)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x1,s15, 1.000)
       deallocate(s15)
c
!ilias: t2^2 diag5
      if(t2diag5.eq.0)goto 6001
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3412(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(s16(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k3*k3
       i3=k1*k1
       call egemm(i1,i2,i3,d1,d2,s16)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n1,n3,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n0,n1,s16,d1)
!       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
!       allocate(z40(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k3
!       i3=k3
!       call egemm(i1,i2,i3,d1,d2,z40)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum               !top 2 switched
     &     + (s16(c,a,e,i)*t2a(e,b,k,j)      !caeiebkj (+0.500)
     &     - s16(b,a,e,i)*t2a(e,c,k,j)       !baeieckj (-0.500)
     &     - s16(c,b,e,i)*t2a(e,a,k,j)       !cbeieakj (-0.500)
     &     + s16(b,a,e,j)*t2a(e,c,k,i)       !baejecki (+0.500)
     &     - s16(c,a,e,j)*t2a(e,b,k,i)       !caejebki (-0.500)
     &     + s16(c,b,e,j)*t2a(e,a,k,i)       !cbejeaki (+0.500)
     &     - s16(b,a,e,k)*t2a(e,c,j,i)       !baekecji (-0.500)
     &     + s16(c,a,e,k)*t2a(e,b,j,i)       !caekebji (+0.500)
     &     - s16(c,b,e,k)*t2a(e,a,j,i))/2.0d0!cbekeaji (-0.500)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+t2diag5*sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum145236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z40,-0.500)
!       call
!     & sum245136(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z40, 0.500)
!       call
!     & sum345126(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z40,-0.500)
!       call
!     & sum146235(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z40, 0.500)
!       call
!     & sum246135(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z40,-0.500)
!       call
!     & sum346125(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z40, 0.500)
!       call
!     & sum156234(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z40,-0.500)
!       call
!     & sum256134(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z40, 0.500)
!       call
!     & sum356124(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z40,-0.500)
!       deallocate(z40)
       deallocate(s16)
c
6001  if(t2diag4.eq.0)goto 6002                                    !ilias: testing the particle version of 3cc
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1423(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s17(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s17)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder3124(n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n0,n1,n0,n1,s17,d1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z41(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,d1,d2,z41)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum               
     &     - s17(a,k,n,i)*t2a(c,b,n,j)     !akicbj (-1.000)
     &     + s17(b,k,n,i)*t2a(c,a,n,j)     !bkicaj (+1.000)
     &     + s17(c,j,n,i)*t2a(b,a,n,k)     !cjibak (+1.000)
     &     - s17(c,k,n,i)*t2a(b,a,n,j)     !ckibaj (-1.000)
     &     - s17(b,j,n,i)*t2a(c,a,n,k)     !bjicak (-1.000)
     &     + s17(a,j,n,i)*t2a(c,b,n,k)     !ajicbk (+1.000)
     &     + s17(a,k,n,j)*t2a(c,b,n,i)     !akjcbi (+1.000)
     &     - s17(b,k,n,j)*t2a(c,a,n,i)     !bkjcai (-1.000)
     &     - s17(c,i,n,j)*t2a(b,a,n,k)     !cijbak (-1.000)
     &     + s17(c,k,n,j)*t2a(b,a,n,i)     !ckjbai (+1.000)
     &     + s17(b,i,n,j)*t2a(c,a,n,k)     !bijcak (+1.000)
     &     - s17(a,i,n,j)*t2a(c,b,n,k)     !aijcbk (-1.000)
     &     - s17(a,j,n,k)*t2a(c,b,n,i)     !ajkcbi (-1.000)
     &     + s17(b,j,n,k)*t2a(c,a,n,i)     !bjkcai (+1.000)
     &     + s17(c,i,n,k)*t2a(b,a,n,j)     !cikbaj (+1.000)
     &     - s17(c,j,n,k)*t2a(b,a,n,i)     !cjkbai (-1.000)
     &     - s17(b,i,n,k)*t2a(c,a,n,j)     !bikcaj (-1.000)
     &     + s17(a,i,n,k)*t2a(c,b,n,j)     !aikcbj (+1.000)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+t2diag4*sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum125346(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41,-1.000)
!       call
!     & sum135246(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41, 1.000)
!       call
!     & sum234156(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41, 1.000)
!       call
!     & sum235146(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41,-1.000)
!       call
!     & sum134256(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41,-1.000)
!       call
!     & sum124356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41, 1.000)
!       call
!     & sum126345(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41, 1.000)
!       call
!     & sum136245(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41,-1.000)
!       call
!     & sum234165(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41,-1.000)
!       call
!     & sum236145(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41, 1.000)
!       call
!     & sum134265(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41, 1.000)
!       call
!     & sum124365(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41,-1.000)
!       call
!     & sum126354(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41,-1.000)
!       call
!     & sum136254(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41, 1.000)
!       call
!     & sum235164(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41, 1.000)
!       call
!     & sum236154(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41,-1.000)
!       call
!     & sum135264(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41,-1.000)
!       call
!     & sum125364(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z41, 1.000)
!       deallocate(z41)
       deallocate(s17)
c
6002  if(t2diag3.eq.0)goto 6003
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))                 !ilias: commented out 13 lines for 3cc
       call reorder2341(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s18(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s18)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3412(n1,n3,n0,n1,n1,n3,n1,n3,
!     & n1,n3,n1,n3,n1,n3,n0,n1,s18,d1)
!       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
!       allocate(z42(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k3
!       i3=k3
!       call egemm(i1,i2,i3,d1,d2,z42)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1                      !ilias: commented out 29 lines for 3cc
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3
             sum=sum               
     &     - s18(a,k,f,c)*t2a(f,b,j,i)     !akcbji (-1.000)
     &     + s18(b,k,f,c)*t2a(f,a,j,i)     !bkcaji (+1.000)
     &     + s18(a,k,f,b)*t2a(f,c,j,i)     !akbcji (+1.000)
     &     - s18(b,k,f,a)*t2a(f,c,j,i)     !bkacji (-1.000)
     &     - s18(c,k,f,b)*t2a(f,a,j,i)     !ckbaji (-1.000)
     &     + s18(c,k,f,a)*t2a(f,b,j,i)     !ckabji (+1.000)
     &     + s18(a,j,f,c)*t2a(f,b,k,i)     !ajcbki (+1.000)
     &     - s18(b,j,f,c)*t2a(f,a,k,i)     !bjcaki (-1.000)
     &     - s18(a,j,f,b)*t2a(f,c,k,i)     !ajbcki (-1.000)
     &     + s18(b,j,f,a)*t2a(f,c,k,i)     !bjacki (+1.000)
     &     + s18(c,j,f,b)*t2a(f,a,k,i)     !cjbaki (+1.000)
     &     - s18(c,j,f,a)*t2a(f,b,k,i)     !cjabki (-1.000)
     &     + s18(b,i,f,c)*t2a(f,a,k,j)     !bicakj (+1.000)
     &     - s18(a,i,f,c)*t2a(f,b,k,j)     !aicbkj (-1.000)
     &     - s18(c,i,f,b)*t2a(f,a,k,j)     !cibakj (-1.000)
     &     + s18(c,i,f,a)*t2a(f,b,k,j)     !ciabkj (+1.000)
     &     + s18(a,i,f,b)*t2a(f,c,k,j)     !aibckj (+1.000)
     &     - s18(b,i,f,a)*t2a(f,c,k,j)     !biackj (-1.000)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+t2diag3*sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum256134(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42,-1.000)
!       call
!     & sum356124(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42, 1.000)
!       call
!     & sum156234(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42, 1.000)
!       call
!     & sum156324(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42,-1.000)
!       call
!     & sum356214(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42,-1.000)
!       call
!     & sum256314(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42, 1.000)
!       call
!     & sum246135(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42, 1.000)
!       call
!     & sum346125(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42,-1.000)
!       call
!     & sum146235(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42,-1.000)
!       call
!     & sum146325(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42, 1.000)
!       call
!     & sum346215(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42, 1.000)
!       call
!     & sum246315(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42,-1.000)
!       call
!     & sum345126(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42, 1.000)
!       call
!     & sum245136(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42,-1.000)
!       call
!     & sum345216(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42,-1.000)
!       call
!     & sum245316(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42, 1.000)
!       call
!     & sum145236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42, 1.000)
!       call
!     & sum145326(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z42,-1.000)
!       deallocate(z42)
       deallocate(s18)                                              !ilias: commented out this line for 3cc
c
6003  if(t2diag5.eq.0)goto 6004
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n1,n3,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s19(n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k1*k1
       i3=k3*k3
       call egemm(i1,i2,i3,d1,d2,s19)
       deallocate(d1)
       deallocate(d2)
c
       factor=0.500*t2diag5
       call sum3412(n0,n1,n1,n3,n0,n1,n0,n1,x1,s19,factor)
       deallocate(s19)
c
6004   call sumx2143(n0,n3,n0,n1,n1,n3,n0,n1,n0,n1,x1,intr, 1.000)
c
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z1(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x1,d2,z1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x1(m,c,j,i)*t2a(b,a,m,k)     !cjibak (+1.000)
     &     - x1(m,b,j,i)*t2a(c,a,m,k)     !bjicak (-1.000)
     &     + x1(m,a,j,i)*t2a(c,b,m,k)     !ajicbk (+1.000)
     &     - x1(m,c,k,i)*t2a(b,a,m,j)     !ckibaj (-1.000)
     &     + x1(m,b,k,i)*t2a(c,a,m,j)     !bkicaj (+1.000)
     &     - x1(m,a,k,i)*t2a(c,b,m,j)     !akicbj (-1.000)
     &     + x1(m,c,k,j)*t2a(b,a,m,i)     !ckjbai (+1.000)
     &     - x1(m,b,k,j)*t2a(c,a,m,i)     !bkjcai (-1.000)
     &     + x1(m,a,k,j)*t2a(c,b,m,i)     !akjcbi (+1.000)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum234156(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z1, 1.000)
!       call
!     & sum134256(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z1,-1.000)
!       call
!     & sum124356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z1, 1.000)
!       call
!     & sum235146(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z1,-1.000)
!       call
!     & sum135246(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z1, 1.000)
!       call
!     & sum125346(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z1,-1.000)
!       call
!     & sum236145(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z1, 1.000)
!       call
!     & sum136245(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z1,-1.000)
!       call
!     & sum126345(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z1, 1.000)
!       deallocate(z1)
       deallocate(x1)
c
      if(t2diag4.eq.0)goto 6005                                    !ilias: testing the particle version of 3cc
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n0,n1,intm,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s20(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s20)
       deallocate(d1)
       deallocate(d2)
c
       factor=t2diag4
       call sum2314(n0,n1,n1,n3,n0,n1,n0,n1,x17,s20,factor)
       deallocate(s20)
c
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z79(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x17,d2,z79)
!       deallocate(d2)
c
6005    do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x17(m,a,k,i)*t2a(c,b,m,j)     !akicbj (+1.000)
     &     - x17(m,b,k,i)*t2a(c,a,m,j)     !bkicaj (-1.000)
     &     + x17(m,c,k,i)*t2a(b,a,m,j)     !ckibaj (+1.000)
     &     - x17(m,a,j,i)*t2a(c,b,m,k)     !ajicbk (-1.000)
     &     + x17(m,b,j,i)*t2a(c,a,m,k)     !bjicak (+1.000)
     &     - x17(m,c,j,i)*t2a(b,a,m,k)     !cjibak (-1.000)
     &     - x17(m,a,k,j)*t2a(c,b,m,i)     !akjcbi (-1.000)
     &     + x17(m,b,k,j)*t2a(c,a,m,i)     !bkjcai (+1.000)
     &     - x17(m,c,k,j)*t2a(b,a,m,i)     !ckjbai (-1.000)
     &     + x17(m,a,j,k)*t2a(c,b,m,i)     !ajkcbi (+1.000)
     &     - x17(m,b,j,k)*t2a(c,a,m,i)     !bjkcai (-1.000)
     &     + x17(m,c,j,k)*t2a(b,a,m,i)     !cjkbai (+1.000)
     &     + x17(m,a,i,j)*t2a(c,b,m,k)     !aijcbk (+1.000)
     &     - x17(m,b,i,j)*t2a(c,a,m,k)     !bijcak (-1.000)
     &     + x17(m,c,i,j)*t2a(b,a,m,k)     !cijbak (+1.000)
     &     - x17(m,a,i,k)*t2a(c,b,m,j)     !aikcbj (-1.000)
     &     + x17(m,b,i,k)*t2a(c,a,m,j)     !bikcaj (+1.000)
     &     - x17(m,c,i,k)*t2a(b,a,m,j)     !cikbaj (-1.000)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum125346(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79, 1.000)
!       call
!     & sum135246(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79,-1.000)
!       call
!     & sum235146(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79, 1.000)
!       call
!     & sum124356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79,-1.000)
!       call
!     & sum134256(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79, 1.000)
!       call
!     & sum234156(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79,-1.000)
!       call
!     & sum126345(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79,-1.000)
!       call
!     & sum136245(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79, 1.000)
!       call
!     & sum236145(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79,-1.000)
!       call
!     & sum126354(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79, 1.000)
!       call
!     & sum136254(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79,-1.000)
!       call
!     & sum236154(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79, 1.000)
!       call
!     & sum124365(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79, 1.000)
!       call
!     & sum134265(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79,-1.000)
!       call
!     & sum234165(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79, 1.000)
!       call
!     & sum125364(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79,-1.000)
!       call
!     & sum135264(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79, 1.000)
!       call
!     & sum235164(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z79,-1.000)
!       deallocate(z79)
       deallocate(x17)
c
      if(t2diag3.eq.0)goto 6006
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,intm,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s21(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s21)
       deallocate(d1)
       deallocate(d2)
c
       factor=-t2diag3
       call sum3412(n1,n3,n1,n3,n1,n3,n0,n1,x18,s21,factor)
       deallocate(s21)
c
!       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
!       allocate(z80(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k3
!       i3=k3
!       call egemm(i1,i2,i3,x18,d2,z80)
!       deallocate(d2)
c
6006    do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     - x18(e,c,a,k)*t2a(e,b,j,i)     !cakbji (-1.000)
     &     + x18(e,c,b,k)*t2a(e,a,j,i)     !cbkaji (+1.000)
     &     + x18(e,b,a,k)*t2a(e,c,j,i)     !bakcji (+1.000)
     &     - x18(e,a,b,k)*t2a(e,c,j,i)     !abkcji (-1.000)
     &     - x18(e,b,c,k)*t2a(e,a,j,i)     !bckaji (-1.000)
     &     + x18(e,a,c,k)*t2a(e,b,j,i)     !ackbji (+1.000)
     &     + x18(e,c,a,j)*t2a(e,b,k,i)     !cajbki (+1.000)
     &     - x18(e,c,b,j)*t2a(e,a,k,i)     !cbjaki (-1.000)
     &     - x18(e,b,a,j)*t2a(e,c,k,i)     !bajcki (-1.000)
     &     + x18(e,a,b,j)*t2a(e,c,k,i)     !abjcki (+1.000)
     &     + x18(e,b,c,j)*t2a(e,a,k,i)     !bcjaki (+1.000)
     &     - x18(e,a,c,j)*t2a(e,b,k,i)     !acjbki (-1.000)
     &     - x18(e,c,a,i)*t2a(e,b,k,j)     !caibkj (-1.000)
     &     + x18(e,c,b,i)*t2a(e,a,k,j)     !cbiakj (+1.000)
     &     + x18(e,b,a,i)*t2a(e,c,k,j)     !baickj (+1.000)
     &     - x18(e,a,b,i)*t2a(e,c,k,j)     !abickj (-1.000)
     &     - x18(e,b,c,i)*t2a(e,a,k,j)     !bciakj (-1.000)
     &     + x18(e,a,c,i)*t2a(e,b,k,j)     !acibkj (+1.000)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum256134(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80,-1.000)
!       call
!     & sum356124(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80, 1.000)
!       call
!     & sum156234(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80, 1.000)
!       call
!     & sum156324(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80,-1.000)
!       call
!     & sum356214(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80,-1.000)
!       call
!     & sum256314(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80, 1.000)
!       call
!     & sum246135(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80, 1.000)
!       call
!     & sum346125(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80,-1.000)
!       call
!     & sum146235(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80,-1.000)
!       call
!     & sum146325(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80, 1.000)
!       call
!     & sum346215(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80, 1.000)
!       call
!     & sum246315(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80,-1.000)
!       call
!     & sum245136(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80,-1.000)
!       call
!     & sum345126(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80, 1.000)
!       call
!     & sum145236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80, 1.000)
!       call
!     & sum145326(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80,-1.000)
!       call
!     & sum345216(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80,-1.000)
!       call
!     & sum245316(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z80, 1.000)
!       deallocate(z80)
       deallocate(x18)
c
       allocate(x19(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       x19=0.0d0
c
      if(t3diag3.eq.0)goto 5001
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder451236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,t3a,f2)
       allocate(s22(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k3*k1*k1
       call egemm(i1,i2,i3,d1,f2,s22)
       deallocate(d1)
       deallocate(f2)
c
       factor=t3diag3
       call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x19,s22,factor)
       deallocate(s22)
c
5001  if(t3diag5.eq.0)goto 5002
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s23(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1
       i3=k3*k3
       call egemm(i1,i2,i3,d1,d2,s23)
       deallocate(d1)
       deallocate(d2)
c
       factor=0.500*t3diag5
       call sum3412(n0,n1,n0,n1,n0,n1,n0,n1,x5,s23,factor)
       deallocate(s23)
c
5002   call sumx2143(n0,n3,n0,n1,n0,n1,n0,n1,n0,n1,x5,intr, 1.000)
c
!       allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder451236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,t3a,f2)
!       allocate(z5(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1*k1
!       i2=k1*k3*k3*k3
!       i3=k1*k1
!       call egemm(i1,i2,i3,x5,f2,z5)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum
     &     + (x5(n,m,j,i)*t3a(c,b,a,n,m,k)      !jicbak (+0.500)
     &     - x5(n,m,k,i)*t3a(c,b,a,n,m,j)       !kicbaj (-0.500)
     &     + x5(n,m,k,j)*t3a(c,b,a,n,m,i))/2.0d0!kjcbai (+0.500)
             enddo;enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       v3a=v3a+0.500*z5
!       call
!     & sum123546(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z5,-0.500)
!       call
!     & sum123645(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z5, 0.500)
!       deallocate(z5)
       deallocate(x5)
c
       allocate(x20(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       x20=0.0d0
c
      if(t3diag4.eq.0)goto 5003
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(s24(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k3*k3*k1
       call egemm(i1,i2,i3,d1,f2,s24)
       deallocate(d1)
       deallocate(f2)
c
       factor=t3diag4
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x20,s24,factor)
       deallocate(s24)
c
5003  if(t3diag1.eq.0)goto 5004
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
      if(t3diag2.eq.0)then
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intm,d1)
      else
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intr,d1)
      endif
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s25(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s25)
       deallocate(d1)
       deallocate(d2)
c
       factor=t3diag1
       call sum3412(n0,n1,n1,n3,n1,n3,n0,n1,x6,s25,factor)
       deallocate(s25)
c
5004  if(t3diag4.eq.0)goto 5005
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(q11(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k3*k3*k1
       call egemm(i1,i2,i3,d1,d2,q11)
       deallocate(d1)
       deallocate(d2)
c
       factor=-0.500*t3diag4
       call sum21(n0,n1,n0,n1,x3,q11,factor)
       deallocate(q11)
c
5005  if(t3diag5.eq.0)goto 5006
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3412(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(s26(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k3*k3
       i3=k1*k1
       call egemm(i1,i2,i3,d1,d2,s26)
       deallocate(d1)
       deallocate(d2)
c
       factor=0.500*t3diag5
       call sum3412(n1,n3,n1,n3,n1,n3,n1,n3,x7,s26,factor)
       deallocate(s26)
c
5006   call sumx4321(n0,n3,n1,n3,n1,n3,n1,n3,n1,n3,x7,intr, 1.000)
c
!       allocate(f2(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder123456(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t3a,f2)
!       allocate(z7(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
!       i1=k3*k3
!       i2=k1*k1*k1*k3
!       i3=k3*k3
!       call egemm(i1,i2,i3,x7,f2,z7)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do f=n1+1,n3
             sum=sum
     &     + (x7(f,e,c,b)*t3a(f,e,a,k,j,i)      !cbakji (+0.500)
     &     - x7(f,e,c,a)*t3a(f,e,b,k,j,i)       !cabkji (-0.500)
     &     + x7(f,e,b,a)*t3a(f,e,c,k,j,i))/2.0d0!backji (+0.500)
             enddo;enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum345612(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z7, 0.500)
!       call
!     & sum245613(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z7,-0.500)
!       call
!     & sum145623(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z7, 0.500)
!       deallocate(z7)
       deallocate(x7)
c
      if(t3diag3.eq.0)goto 5007
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3412(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(q12(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k3*k1*k1
       call egemm(i1,i2,i3,d1,d2,q12)
       deallocate(d1)
       deallocate(d2)
c
       factor=0.500*t3diag3
       call sum21(n1,n3,n1,n3,x4,q12,factor)
       deallocate(q12)
c
       allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)
       allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder451236(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,n1,n3,n0,n1,t3b,f2)
       allocate(s27(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k4*k1*k2
       call egemm(i1,i2,i3,d1,f2,s27)
       deallocate(d1)
       deallocate(f2)
c
       factor=2.000*t3diag3
       call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x19,s27,factor)
       deallocate(s27)
c
!       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
!       allocate(z46(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k3
!       i3=k3
!       call egemm(i1,i2,i3,x19,d2,z46)
!       deallocate(d2)
c
5007   do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + (x19(e,b,a,k)*t2a(e,c,j,i)      !bakcji (+0.500)
     &     - x19(e,c,a,k)*t2a(e,b,j,i)       !cakbji (-0.500)
     &     + x19(e,c,b,k)*t2a(e,a,j,i)       !cbkaji (+0.500)
     &     - x19(e,b,a,j)*t2a(e,c,k,i)       !bajcki (-0.500)
     &     + x19(e,c,a,j)*t2a(e,b,k,i)       !cajbki (+0.500)
     &     - x19(e,c,b,j)*t2a(e,a,k,i)       !cbjaki (-0.500)
     &     + x19(e,b,a,i)*t2a(e,c,k,j)       !baickj (+0.500)
     &     - x19(e,c,a,i)*t2a(e,b,k,j)       !caibkj (-0.500)
     &     + x19(e,c,b,i)*t2a(e,a,k,j))/2.0d0!cbiakj (+0.500)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum156234(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z46, 0.500)
!       call
!     & sum256134(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z46,-0.500)
!       call
!     & sum356124(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z46, 0.500)
!       call
!     & sum146235(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z46,-0.500)
!       call
!     & sum246135(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z46, 0.500)
!       call
!     & sum346125(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z46,-0.500)
!       call
!     & sum145236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z46, 0.500)
!       call
!     & sum245136(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z46,-0.500)
!       call
!     & sum345126(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z46, 0.500)
!       deallocate(z46)
       deallocate(x19)
c
      if(t3diag4.eq.0)goto 5008
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,intm,d1)
       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(s28(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k3*k4*k2
       call egemm(i1,i2,i3,d1,f2,s28)
       deallocate(d1)
       deallocate(f2)
c
       factor=2.000*t3diag4
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x20,s28, 2.000)
       deallocate(s28)
c
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z48(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x20,d2,z48)
!       deallocate(d2)
c
5008   do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + (x20(m,a,k,j)*t2a(c,b,m,i)      !akjcbi (+0.500)
     &     - x20(m,b,k,j)*t2a(c,a,m,i)       !bkjcai (-0.500)
     &     + x20(m,c,k,j)*t2a(b,a,m,i)       !ckjbai (+0.500)
     &     - x20(m,a,k,i)*t2a(c,b,m,j)       !akicbj (-0.500)
     &     + x20(m,b,k,i)*t2a(c,a,m,j)       !bkicaj (+0.500)
     &     - x20(m,c,k,i)*t2a(b,a,m,j)       !ckibaj (-0.500)
     &     + x20(m,a,j,i)*t2a(c,b,m,k)       !ajicbk (+0.500)
     &     - x20(m,b,j,i)*t2a(c,a,m,k)       !bjicak (-0.500)
     &     + x20(m,c,j,i)*t2a(b,a,m,k))/2.0d0!cjibak (+0.500)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum126345(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z48, 0.500)
!       call
!     & sum136245(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z48,-0.500)
!       call
!     & sum236145(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z48, 0.500)
!       call
!     & sum125346(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z48,-0.500)
!       call
!     & sum135246(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z48, 0.500)
!       call
!     & sum235146(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z48,-0.500)
!       call
!     & sum124356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z48, 0.500)
!       call
!     & sum134256(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z48,-0.500)
!       call
!     & sum234156(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z48, 0.500)
!       deallocate(z48)
       deallocate(x20)
c
      if(t3diag1.eq.0)goto 5009
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n2,n3,intm,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s29(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s29)
       deallocate(d1)
       deallocate(d2)
c
       factor=t3diag1
       call sum3412(n0,n2,n2,n3,n1,n3,n0,n1,x8,s29,factor)
       deallocate(s29)

       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n1,n3,intm,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s30(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s30)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n0,n1,n1,n3,n1,n3,n0,n1,x6,s30,factor)
       deallocate(s30)
c
5009   call sumx3142(n0,n3,n0,n1,n1,n3,n1,n3,n0,n1,x6,intr, 1.000)
c
!       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
!       allocate(z6(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
!       i1=k1*k3
!       i2=k1*k1*k3*k3
!       i3=k3*k1
!       call egemm(i1,i2,i3,x6,f2,z6)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + x6(m,e,c,i)*t3a(e,b,a,m,k,j)   !cibakj (+1.000)
     &     - x6(m,e,b,i)*t3a(e,c,a,m,k,j)   !bicakj (-1.000)
     &     + x6(m,e,a,i)*t3a(e,c,b,m,k,j)   !aicbkj (+1.000)
     &     - x6(m,e,c,j)*t3a(e,b,a,m,k,i)   !cjbaki (-1.000)
     &     + x6(m,e,b,j)*t3a(e,c,a,m,k,i)   !bjcaki (+1.000)
     &     - x6(m,e,a,j)*t3a(e,c,b,m,k,i)   !ajcbki (-1.000)
     &     + x6(m,e,c,k)*t3a(e,b,a,m,j,i)   !ckbaji (+1.000)
     &     - x6(m,e,b,k)*t3a(e,c,a,m,j,i)   !bkcaji (-1.000)
     &     + x6(m,e,a,k)*t3a(e,c,b,m,j,i)   !akcbji (+1.000)
             enddo;enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum234516(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z6, 1.000)
!       call
!     & sum134526(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z6,-1.000)
!       call
!     & sum124536(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z6, 1.000)
!       call
!     & sum234615(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z6,-1.000)
!       call
!     & sum134625(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z6, 1.000)
!       call
!     & sum124635(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z6,-1.000)
!       call
!     & sum235614(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z6, 1.000)
!       call
!     & sum135624(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z6,-1.000)
!       call
!     & sum125634(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z6, 1.000)
!       deallocate(z6)
       deallocate(x6)
c
      if(t3diag4.eq.0)goto 5010
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,intm,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(q13(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k3*k4*k2
       call egemm(i1,i2,i3,d1,d2,q13)
       deallocate(d1)
       deallocate(d2)
c
       factor=t3diag4
       call sum21(n0,n1,n0,n1,x3,q13,factor)
       deallocate(q13)
c
5010   call sumx12(0,n3,n0,n1,n0,n1,x3,fockr, 1.000)
c
!       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
!       allocate(z3(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       i1=k1
!       i2=k1*k1*k3*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x3,f2,z3)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - x3(m,i)*t3a(c,b,a,m,k,j)     !icbakj (-1.000)
     &     + x3(m,j)*t3a(c,b,a,m,k,i)     !jcbaki (+1.000)
     &     - x3(m,k)*t3a(c,b,a,m,j,i)     !kcbaji (-1.000)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       v3a=v3a-z3
!       call
!     & sum123465(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z3, 1.000)
!       call
!     & sum123564(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z3,-1.000)
!       deallocate(z3)
       deallocate(x3)
c
      if(t3diag3.eq.0)goto 5011
       allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)
       allocate(d2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
       call reorder3412(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n2,n3,n1,n3,t2b,d2)
       allocate(q14(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k4*k1*k2
       call egemm(i1,i2,i3,d1,d2,q14)
       deallocate(d1)
       deallocate(d2)
c
       factor=-t3diag3
       call sum21(n1,n3,n1,n3,x4,q14,factor)
       deallocate(q14)
c
5011   call sumx21(0,n3,n1,n3,n1,n3,x4,fockr, 1.000)
c
!       allocate(f2(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder123456(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t3a,f2)
!       allocate(z4(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3))
!       i1=k3
!       i2=k1*k1*k1*k3*k3
!       i3=k3
!       call egemm(i1,i2,i3,x4,f2,z4)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + x4(e,c)*t3a(e,b,a,k,j,i)     !cbakji (+1.000)
     &     - x4(e,b)*t3a(e,c,a,k,j,i)     !bcakji (-1.000)
     &     + x4(e,a)*t3a(e,c,b,k,j,i)     !acbkji (+1.000)
             enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum234561(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z4, 1.000)
!       call
!     & sum134562(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z4,-1.000)
!       call
!     & sum124563(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z4, 1.000)
!       deallocate(z4)
       deallocate(x4)
c
      if(t3diag1.eq.0)goto 5012
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
      if(t3diag2.eq.0)then
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intm,d1)
      else
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intb,d1)
      endif
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s31(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s31)
       deallocate(d1)
       deallocate(d2)
c
       factor=t3diag1
       call sum3412(n0,n2,n2,n3,n1,n3,n0,n1,x8,s31,factor)
       deallocate(s31)
c
5012   call sumx3142(n0,n3,n0,n2,n2,n3,n1,n3,n0,n1,x8,intm, 1.000)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z8(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
!       i1=k1*k3
!       i2=k1*k1*k3*k3
!       i3=k4*k2
!       call egemm(i1,i2,i3,x8,f2,z8)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1
        if(indocc(k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x8(m,e,c,i)*t3b(e,b,a,m,k,j)   !cibakj (+1.000)
     &     - x8(m,e,b,i)*t3b(e,c,a,m,k,j)   !bicakj (-1.000)
     &     + x8(m,e,a,i)*t3b(e,c,b,m,k,j)   !aicbkj (+1.000)
     &     - x8(m,e,c,j)*t3b(e,b,a,m,k,i)   !cjbaki (-1.000)
     &     + x8(m,e,b,j)*t3b(e,c,a,m,k,i)   !bjcaki (+1.000)
     &     - x8(m,e,a,j)*t3b(e,c,b,m,k,i)   !ajcbki (-1.000)
     &     + x8(m,e,c,k)*t3b(e,b,a,m,j,i)   !ckbaji (+1.000)
     &     - x8(m,e,b,k)*t3b(e,c,a,m,j,i)   !bkcaji (-1.000)
     &     + x8(m,e,a,k)*t3b(e,c,b,m,j,i)   !akcbji (+1.000)
             enddo;enddo
             v3a(c,b,a,k,j,i)=v3a(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum234516(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z8, 1.000)
!       call
!     & sum134526(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z8,-1.000)
!       call
!     & sum124536(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z8, 1.000)
!       call
!     & sum234615(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z8,-1.000)
!       call
!     & sum134625(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z8, 1.000)
!       call
!     & sum124635(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z8,-1.000)
!       call
!     & sum235614(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z8, 1.000)
!       call
!     & sum135624(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z8,-1.000)
!       call
!     & sum125634(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,v3a,z8, 1.000)
!       deallocate(z8)
       deallocate(x8)
c
       do i=n0+1,n1-2
       do j=i+1,n1-1
       do k=j+1,n1
       do a=n1+1,n3-2
       do b=a+1,n3-1
       do c=b+1,n3
!
!        iocca=0
!        iunoa=0
!        if(i.gt.(n1-iactocca))iocca=iocca+1
!        if(j.gt.(n1-iactocca))iocca=iocca+1
!        if(k.gt.(n1-iactocca))iocca=iocca+1
!        if(iocca.lt.iactindt)cycle
!        if(a.lt.(n1+iactunoa+1))iunoa=iunoa+1
!        if(b.lt.(n1+iactunoa+1))iunoa=iunoa+1
!        if(c.lt.(n1+iactunoa+1))iunoa=iunoa+1
!        if(iunoa.lt.iactindt)cycle
!
         coeleft=fockr(c,c)
     &          +fockr(b,b)
     &          +fockr(a,a)
     &          -fockr(k,k)
     &          -fockr(j,j)
     &          -fockr(i,i)
     &          +shift
         t3a(c,b,a,k,j,i)=t3a(c,b,a,k,j,i)-v3a(c,b,a,k,j,i)/coeleft
         t3a(c,b,a,k,i,j)=-t3a(c,b,a,k,j,i)
         t3a(c,b,a,i,j,k)=-t3a(c,b,a,k,j,i)
         t3a(c,b,a,i,k,j)= t3a(c,b,a,k,j,i)
         t3a(c,b,a,j,k,i)=-t3a(c,b,a,k,j,i)
         t3a(c,b,a,j,i,k)= t3a(c,b,a,k,j,i)
         t3a(c,a,b,k,j,i)=-t3a(c,b,a,k,j,i)
         t3a(c,a,b,k,i,j)= t3a(c,b,a,k,j,i)
         t3a(c,a,b,i,j,k)= t3a(c,b,a,k,j,i)
         t3a(c,a,b,i,k,j)=-t3a(c,b,a,k,j,i)
         t3a(c,a,b,j,k,i)= t3a(c,b,a,k,j,i)
         t3a(c,a,b,j,i,k)=-t3a(c,b,a,k,j,i)
         t3a(a,b,c,k,j,i)=-t3a(c,b,a,k,j,i)
         t3a(a,b,c,k,i,j)= t3a(c,b,a,k,j,i)
         t3a(a,b,c,i,j,k)= t3a(c,b,a,k,j,i)
         t3a(a,b,c,i,k,j)=-t3a(c,b,a,k,j,i)
         t3a(a,b,c,j,k,i)= t3a(c,b,a,k,j,i)
         t3a(a,b,c,j,i,k)=-t3a(c,b,a,k,j,i)
         t3a(a,c,b,k,j,i)= t3a(c,b,a,k,j,i)
         t3a(a,c,b,k,i,j)=-t3a(c,b,a,k,j,i)
         t3a(a,c,b,i,j,k)=-t3a(c,b,a,k,j,i)
         t3a(a,c,b,i,k,j)= t3a(c,b,a,k,j,i)
         t3a(a,c,b,j,k,i)=-t3a(c,b,a,k,j,i)
         t3a(a,c,b,j,i,k)= t3a(c,b,a,k,j,i)
         t3a(b,c,a,k,j,i)=-t3a(c,b,a,k,j,i)
         t3a(b,c,a,k,i,j)= t3a(c,b,a,k,j,i)
         t3a(b,c,a,i,j,k)= t3a(c,b,a,k,j,i)
         t3a(b,c,a,i,k,j)=-t3a(c,b,a,k,j,i)
         t3a(b,c,a,j,k,i)= t3a(c,b,a,k,j,i)
         t3a(b,c,a,j,i,k)=-t3a(c,b,a,k,j,i)
         t3a(b,a,c,k,j,i)= t3a(c,b,a,k,j,i)
         t3a(b,a,c,k,i,j)=-t3a(c,b,a,k,j,i)
         t3a(b,a,c,i,j,k)=-t3a(c,b,a,k,j,i)
         t3a(b,a,c,i,k,j)= t3a(c,b,a,k,j,i)
         t3a(b,a,c,j,k,i)=-t3a(c,b,a,k,j,i)
         t3a(b,a,c,j,i,k)= t3a(c,b,a,k,j,i)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
c
       end
