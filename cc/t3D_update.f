       subroutine t3d_update(n0,n1,n2,n3,k1,k2,k3,k4,lvl1,lvlq,shift,v3d
     & ,fockr,fockb,intr,intb,intm,t1a,t1b,t2a,t2b,t2c,t3a,t3b,t3c,t3d,
     & iactoccb,iactunob,iactindt,
     & t2diag3,t2diag4,t2diag5,t3diag1,t3diag2,t3diag3,t3diag4,t3diag5)
!     & t4a,t4b,t4c,t4d,t4e)
c
       integer a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p
       integer iactoccb,iactunob,iactindt
       integer ioccb,iunob
       integer,allocatable::indocc(:,:,:)
       integer,allocatable::indunocc(:,:,:)
       character lvl1*6,lvlq*6
!       integer indocc(n0+1:n2,n0+1:n2,n0+1:n2)
!       integer indunocc(n2+1:n3,n2+1:n3,n2+1:n3)
       real t2diag3,t2diag4,t2diag5
       real t3diag1,t3diag2,t3diag3,t3diag4,t3diag5
       real factor
       real*8 shift,pp,coeleft,time1,time2,timt1,timt2
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
       real*8 v3d(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2)
c
!       real*8,allocatable::t4a(:,:)
!       real*8,allocatable::t4b(:,:)
!       real*8,allocatable::t4c(:,:)
       real*8,allocatable::t4d(:,:,:,:,:,:,:,:)                     !ilias: if no quadruples comment out the following 5 lines
       real*8,allocatable::t4e(:,:,:,:,:,:,:,:)
c
       integer ta,tb,tc,td,te
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
       real*8,allocatable::q1(:,:)
       real*8,allocatable::q2(:,:)
       real*8,allocatable::q3(:,:)
       real*8,allocatable::q4(:,:)
       real*8,allocatable::s32(:,:,:,:)
       real*8,allocatable::q16(:,:)
       real*8,allocatable::q15(:,:)
       real*8,allocatable::s1(:,:,:,:)
       real*8,allocatable::s2(:,:,:,:)
       real*8,allocatable::s3(:,:,:,:)
       real*8,allocatable::s4(:,:,:,:)
       real*8,allocatable::s5(:,:,:,:)
       real*8,allocatable::s6(:,:,:,:)
       real*8,allocatable::q5(:,:)
       real*8,allocatable::q6(:,:)
       real*8,allocatable::s7(:,:,:,:)
       real*8,allocatable::s33(:,:,:,:)
       real*8,allocatable::s8(:,:,:,:)
       real*8,allocatable::s34(:,:,:,:)
       real*8,allocatable::q7(:,:)
       real*8,allocatable::s9(:,:,:,:)
       real*8,allocatable::s36(:,:,:,:)
       real*8,allocatable::s35(:,:,:,:)
       real*8,allocatable::s10(:,:,:,:)
       real*8,allocatable::q8(:,:)
       real*8,allocatable::s11(:,:,:,:)
       real*8,allocatable::s41(:,:,:,:)
       real*8,allocatable::s37(:,:,:,:)
       real*8,allocatable::s12(:,:,:,:)
       real*8,allocatable::s42(:,:,:,:)
       real*8,allocatable::q9(:,:)
       real*8,allocatable::s13(:,:,:,:)
       real*8,allocatable::s45(:,:,:,:)
       real*8,allocatable::s44(:,:,:,:)
       real*8,allocatable::s43(:,:,:,:)
       real*8,allocatable::q17(:,:)
       real*8,allocatable::s39(:,:,:,:)
       real*8,allocatable::s51(:,:,:,:)
       real*8,allocatable::s38(:,:,:,:)
       real*8,allocatable::s50(:,:,:,:)
       real*8,allocatable::s14(:,:,:,:)
       real*8,allocatable::s48(:,:,:,:)
       real*8,allocatable::s47(:,:,:,:)
       real*8,allocatable::s46(:,:,:,:)
       real*8,allocatable::q18(:,:)
       real*8,allocatable::s40(:,:,:,:)
       real*8,allocatable::q10(:,:)
       real*8,allocatable::s49(:,:,:,:)
       real*8,allocatable::s15(:,:,:,:)
       real*8,allocatable::s16(:,:,:,:)
       real*8,allocatable::s17(:,:,:,:)
       real*8,allocatable::s18(:,:,:,:)
       real*8,allocatable::q11(:,:)
       real*8,allocatable::q12(:,:)
       real*8,allocatable::s19(:,:,:,:)
       real*8,allocatable::s20(:,:,:,:)
       real*8,allocatable::s21(:,:,:,:)
       real*8,allocatable::s22(:,:,:,:)
       real*8,allocatable::s23(:,:,:,:)
       real*8,allocatable::s24(:,:,:,:)
       real*8,allocatable::s25(:,:,:,:)
       real*8,allocatable::s26(:,:,:,:)
       real*8,allocatable::s27(:,:,:,:)
       real*8,allocatable::s28(:,:,:,:)
       real*8,allocatable::s29(:,:,:,:)
       real*8,allocatable::s30(:,:,:,:)
       real*8,allocatable::q13(:,:)
       real*8,allocatable::s31(:,:,:,:)
       real*8,allocatable::q14(:,:)
       real*8,allocatable::x1(:,:,:,:)
       real*8,allocatable::z1(:,:,:,:,:,:)
       real*8,allocatable::x2(:,:,:,:)
       real*8,allocatable::z2(:,:,:,:,:,:)
       real*8,allocatable::x3(:,:,:,:)
       real*8,allocatable::z3(:,:,:,:,:,:)
       real*8,allocatable::x4(:,:)
       real*8,allocatable::z4(:,:,:,:,:,:)
       real*8,allocatable::x5(:,:)
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
       real*8,allocatable::z20(:,:,:,:,:,:)
       real*8,allocatable::x16(:,:,:,:)
       real*8,allocatable::z21(:,:,:,:,:,:)
       real*8,allocatable::z27(:,:,:,:,:,:)
       real*8,allocatable::z31(:,:,:,:,:,:)
       real*8,allocatable::z77(:,:,:,:,:,:)
       real*8,allocatable::z76(:,:,:,:,:,:)
       real*8,allocatable::z75(:,:,:,:,:,:)
       real*8,allocatable::z79(:,:,:,:,:,:)
       real*8,allocatable::x17(:,:,:,:)
       real*8,allocatable::z78(:,:,:,:,:,:)
       real*8,allocatable::z46(:,:,:,:,:,:)
       real*8,allocatable::z47(:,:,:,:,:,:)
       real*8,allocatable::z48(:,:,:,:,:,:)
       real*8,allocatable::z51(:,:,:,:,:,:)
       real*8,allocatable::z53(:,:,:,:,:,:)
       real*8,allocatable::z55(:,:,:,:,:,:)
!npb
       real*8,allocatable::x999(:,:,:,:)
       real*8,allocatable::z999(:,:,:,:,:,:)
c
       allocate(indocc(n0+1:n2,n0+1:n2,n0+1:n2))
       allocate(indunocc(n2+1:n3,n2+1:n3,n2+1:n3))
       indocc=0
       indunocc=0
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        ioccb=0
        if(i.gt.(n2-iactoccb))ioccb=ioccb+1
        if(j.gt.(n2-iactoccb))ioccb=ioccb+1
        if(k.gt.(n2-iactoccb))ioccb=ioccb+1
        if(ioccb.lt.iactindt)indocc(k,j,i)=1
       enddo;enddo;enddo
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          iunob=0
          if(a.lt.(n2+iactunob+1))iunob=iunob+1
          if(b.lt.(n2+iactunob+1))iunob=iunob+1
          if(c.lt.(n2+iactunob+1))iunob=iunob+1
          if(iunob.lt.iactindt)indunocc(c,b,a)=1
          enddo;enddo;enddo
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q1(n0+1:n2,n0+1:n2))
       i1=k2*k2
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q1)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x4(n0+1:n2,n0+1:n2))
       x4=0.0d0
       x4=x4+q1
       deallocate(q1)
c
       allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n2,n3,n2,n3,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q2(n2+1:n3,n2+1:n3))
       i1=k4*k4
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q2)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x5(n2+1:n3,n2+1:n3))
       x5=0.0d0
       x5=x5+q2
       deallocate(q2)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q3(n0+1:n1,n1+1:n3))
       i1=k3*k1
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q3)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x9(n0+1:n1,n1+1:n3))
       x9=0.0d0
       x9=x9+q3
       deallocate(q3)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n2,n3,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q4(n0+1:n2,n2+1:n3))
       i1=k4*k2
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q4)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x12(n0+1:n2,n2+1:n3))
       x12=0.0d0
       x12=x12+q4
c
       allocate(b1(n2+1:n3,n0+1:n2))
       call reorder21(n0,n2,n2,n3,
     & n2,n3,n0,n2,q4,b1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(s32(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,b1,d2,s32)
       deallocate(b1)
       deallocate(d2)
c
       allocate(x1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       x1=0.0d0
       call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x1,s32, 1.000)
       deallocate(s32)
c
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q16(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,q4,b2,q16)
       deallocate(b2)
c
       call sum21(n2,n3,n2,n3,x5,q16,-1.000)
       deallocate(q16)
c
       allocate(b1(n2+1:n3,n0+1:n2))
       call reorder21(n0,n2,n2,n3,
     & n2,n3,n0,n2,q4,b1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q15(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,b1,b2,q15)
       deallocate(b1)
       deallocate(b2)
       deallocate(q4)
c
       call sum21(n0,n2,n0,n2,x4,q15, 1.000)
       deallocate(q15)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s1)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n0,n2,n2,n3,n0,n2,n0,n2,x1,s1,-1.000)
       deallocate(s1)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s2(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s2)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x15(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       x15=0.0d0
       call sum3124(n0,n2,n2,n3,n0,n2,n0,n2,x15,s2, 1.000)
       deallocate(s2)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s3(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s3)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x16(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       x16=0.0d0
       call sum3124(n2,n3,n2,n3,n2,n3,n0,n2,x16,s3, 1.000)
       deallocate(s3)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n2,n3,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s4(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4*k4
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s4)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x2(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       x2=0.0d0
       call sum4123(n2,n3,n2,n3,n2,n3,n0,n2,x2,s4, 1.000)
       deallocate(s4)
c
       allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n1,n3,n0,n2,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s5(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s5)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x3(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       x3=0.0d0
       call sum3124(n0,n1,n1,n3,n2,n3,n0,n2,x3,s5,-1.000)
       deallocate(s5)
c
       allocate(d1(n2+1:n3,n0+1:n1,n1+1:n3,n2+1:n3))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n1,n3,n2,n3,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s6(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))
       i1=k4*k3*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s6)
       deallocate(d1)
       deallocate(b2)
c
       call sum4123(n0,n1,n1,n3,n2,n3,n0,n2,x3,s6, 1.000)
       deallocate(s6)
c
       allocate(b1(n2+1:n3,n0+1:n2))
       call reorder12(0,n3,0,n3,
     & n2,n3,n0,n2,fockb,b1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q5(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,b1,b2,q5)
       deallocate(b1)
       deallocate(b2)
c
       call sum21(n0,n2,n0,n2,x4,q5, 1.000)
       deallocate(q5)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n2,n2,n3,fockb,b1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q6(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,b1,b2,q6)
       deallocate(b1)
       deallocate(b2)
c
       call sum21(n2,n3,n2,n3,x5,q6,-1.000)
       deallocate(q6)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s7(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s7)
       deallocate(d1)
       deallocate(b2)
c
!       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
!       call reorder2314(n0,n2,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n0,n2,n0,n2,n0,n2,s7,d1)
!       allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder451236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,t3d,f2)
!       allocate(z27(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
!       i1=k2*k2
!       i2=k2*k4*k4*k4
!       i3=k2*k2
!       call egemm(i1,i2,i3,d1,f2,z27)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2;do n=n0+1,n2
             sum=sum
     &     + (s7(j,n,m,i)*t3d(c,b,a,n,m,k)      !jicbak (+0.500)
     &     - s7(k,n,m,i)*t3d(c,b,a,n,m,j)       !kicbaj (-0.500)
     &     - s7(i,n,m,j)*t3d(c,b,a,n,m,k)       !ijcbak (-0.500)
     &     + s7(i,n,m,k)*t3d(c,b,a,n,m,j)       !ikcbaj (+0.500)
     &     + s7(k,n,m,j)*t3d(c,b,a,n,m,i)       !kjcbai (+0.500)
     &     - s7(j,n,m,k)*t3d(c,b,a,n,m,i))/2.0d0!jkcbai (-0.500)
             enddo;enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       v3d=v3d+0.500*z27
!       call
!     & sum123546(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z27,-0.500)
!       call
!     & sum123465(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z27,-0.500)
!       call
!     & sum123564(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z27, 0.500)
!       call
!     & sum123645(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z27, 0.500)
!       call
!     & sum123654(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z27,-0.500)
!       deallocate(z27)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder3214(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s7,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s33(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s33)
       deallocate(d1)
       deallocate(b2)
       deallocate(s7)
c
       call sum2134(n0,n2,n2,n3,n0,n2,n0,n2,x15,s33,-1.000)
       deallocate(s33)
c
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s8(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s8)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x7(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       x7=0.0d0
       call sum3124(n0,n2,n2,n3,n2,n3,n0,n2,x7,s8,-1.000)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder2314(n2,n3,n0,n2,n2,n3,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,s8,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s34(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s34)
       deallocate(d1)
       deallocate(b2)
       deallocate(s8)
c
       call sum2134(n2,n3,n2,n3,n2,n3,n0,n2,x2,s34, 1.000)
       deallocate(s34)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1423(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q7(n0+1:n2,n0+1:n2))
       i1=k2*k2
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q7)
       deallocate(d1)
       deallocate(b2)
c
       x4=x4-q7
       deallocate(q7)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s9(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       i1=k4*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s9)
       deallocate(d1)
       deallocate(b2)
c
       call sum4123(n0,n2,n2,n3,n2,n3,n0,n2,x7,s9, 1.000)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder2341(n0,n2,n0,n2,n2,n3,n2,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,s9,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s36(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s36)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n2,n3,n2,n3,n2,n3,n0,n2,x16,s36, 1.000)
       deallocate(s36)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3241(n0,n2,n0,n2,n2,n3,n2,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,s9,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s35(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s35)
       deallocate(d1)
       deallocate(b2)
       deallocate(s9)
c
       call sum3124(n0,n2,n2,n3,n0,n2,n0,n2,x1,s35, 1.000)
       deallocate(s35)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s10(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s10)
       deallocate(d1)
       deallocate(b2)
c
!       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
!       call reorder2341(n2,n3,n2,n3,n2,n3,n2,n3,
!     & n2,n3,n2,n3,n2,n3,n2,n3,s10,d1)
!       allocate(f2(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
!       call reorder123456(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,t3d,f2)
!       allocate(z31(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
!       i1=k4*k4
!       i2=k2*k2*k2*k4
!       i3=k4*k4
!       call egemm(i1,i2,i3,d1,f2,z31)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do f=n2+1,n3
             sum=sum
     &     + (s10(b,f,e,c)*t3d(f,e,a,k,j,i)      !bcakji (+0.500)
     &     - s10(a,f,e,c)*t3d(f,e,b,k,j,i)       !acbkji (-0.500)
     &     - s10(c,f,e,b)*t3d(f,e,a,k,j,i)       !cbakji (-0.500)
     &     + s10(c,f,e,a)*t3d(f,e,b,k,j,i)       !cabkji (+0.500)
     &     + s10(a,f,e,b)*t3d(f,e,c,k,j,i)       !abckji (+0.500)
     &     - s10(b,f,e,a)*t3d(f,e,c,k,j,i))/2.0d0!backji (-0.500)
             enddo;enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum345612(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z31, 0.500)
!       call
!     & sum245613(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z31,-0.500)
!       call
!     & sum345621(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z31,-0.500)
!       call
!     & sum245631(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z31, 0.500)
!       call
!     & sum145623(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z31, 0.500)
!       call
!     & sum145632(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z31,-0.500)
!       deallocate(z31)
       deallocate(s10)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder2341(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q8(n2+1:n3,n2+1:n3))
       i1=k4*k4
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q8)
       deallocate(d1)
       deallocate(b2)
c
       x5=x5-q8
       deallocate(q8)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n1,n3,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s11(n0+1:n2,n0+1:n2,n0+1:n1,n1+1:n3))
       i1=k3*k1*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s11)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x10(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       x10=0.0d0
       call sum4123(n0,n2,n0,n1,n1,n3,n0,n2,x10,s11, 1.000)
c
       call sumx2134(n0,n3,n0,n2,n0,n1,n1,n3,n0,n2,x10,intm, 1.000)
c
      if(lvlq.eq.'ccsdtq'.or.lvlq.eq.'4cc')then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       rewind(td)
       read(td)t4d
!       allocate(h2(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       call reorder58412367(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n2,n0,n1,n1,n3,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t4d,h2)
!       allocate(z10(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
!       i1=k2
!       i2=k2*k2*k4*k4*k4
!       i3=k3*k1*k2
!       call egemm(i1,i2,i3,x10,h2,z10)
!       deallocate(h2)
!       deallocate(t4d)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2                     !ilias: if no quadruples comment out the following 14 lines
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     - x10(n,m,e,i)*t4d(c,b,a,e,n,k,j,m) !icbakj(-1.000)
     &     + x10(n,m,e,j)*t4d(c,b,a,e,n,k,i,m) !jcbaki(+1.000)
     &     - x10(n,m,e,k)*t4d(c,b,a,e,n,j,i,m) !kcbaji(-1.000)
             enddo;enddo;enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       v3d=v3d-z10
!       call
!     & sum123465(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z10, 1.000)
!       call
!     & sum123564(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z10,-1.000)
!       deallocate(z10)
       deallocate(t4d)                                              !ilias: if no quadruples comment out the following 2 lines
       deallocate(x10)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      endif
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder3421(n0,n2,n0,n2,n0,n1,n1,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,s11,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(s41(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s41)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n0,n2,n2,n3,n0,n2,n0,n2,x15,s41,-1.000)
       deallocate(s41)
c
       allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder2341(n0,n2,n0,n2,n0,n1,n1,n3,
     & n0,n2,n0,n1,n1,n3,n0,n2,s11,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s37(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s37)
       deallocate(d1)
       deallocate(b2)
       deallocate(s11)
c
       call sum3124(n0,n1,n1,n3,n2,n3,n0,n2,x3,s37,-1.000)
       deallocate(s37)
c
       allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s12(n2+1:n3,n0+1:n1,n2+1:n3,n1+1:n3))
       i1=k3*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s12)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x11(n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3))
       x11=0.0d0
       call sum4123(n0,n1,n2,n3,n1,n3,n2,n3,x11,s12,-1.000)
c
       call sumx1432(n0,n3,n0,n1,n2,n3,n1,n3,n2,n3,x11,intm, 1.000)
c
      if(lvlq.eq.'ccsdtq'.or.lvlq.eq.'4cc')then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       rewind(td)
       read(td)t4d
!       allocate(h2(n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n2))
!       call reorder81423567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n1,n2,n3,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,t4d,h2)
!       allocate(z11(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3))
!       i1=k4
!       i2=k2*k2*k2*k4*k4
!       i3=k3*k4*k1
!       call egemm(i1,i2,i3,x11,h2,z11)
!       deallocate(h2)
!       deallocate(t4d)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2                     !ilias: if no quadruples comment out the following 14 lines
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do f=n2+1,n3;do m=n0+1,n1
             sum=sum
     &     + x11(m,f,e,c)*t4d(f,b,a,e,k,j,i,m) !cbakji(+1.000)
     &     - x11(m,f,e,b)*t4d(f,c,a,e,k,j,i,m) !bcakji(-1.000)
     &     + x11(m,f,e,a)*t4d(f,c,b,e,k,j,i,m) !acbkji(+1.000)
             enddo;enddo;enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum234561(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z11, 1.000)
!       call
!     & sum134562(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z11,-1.000)
!       call
!     & sum124563(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z11, 1.000)
!       deallocate(z11)
       deallocate(t4d)                                              !ilias: if no quadruples comment out the following 2 lines
       deallocate(x11)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      endif
c
       allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3))
       call reorder2431(n2,n3,n0,n1,n2,n3,n1,n3,
     & n0,n1,n1,n3,n2,n3,n2,n3,s12,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(s42(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s42)
       deallocate(d1)
       deallocate(d2)
       deallocate(s12)
c
       call sum3412(n2,n3,n2,n3,n2,n3,n0,n2,x16,s42,-1.000)
       deallocate(s42)
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
       deallocate(q9)
c
       call sumx12(0,n3,n0,n1,n1,n3,x9,fockr, 1.000)
c
      if(lvlq.eq.'ccsdtq'.or.lvlq.eq.'4cc')then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
       rewind(td)
       read(td)t4d
!       allocate(h2(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n2))
!       call reorder84123567(n2,n3,n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n1,n0,n1,n1,n3,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,t4d,h2)
!       allocate(z9(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
!       i2=k2*k2*k2*k4*k4*k4
!       i3=k3*k1
!       call egemm2(i2,i3,x9,h2,z9)
!       deallocate(h2)
!       deallocate(t4d)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2                     !ilias: if no quadruples comment out the following 12 lines
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + x9(m,e)*t4d(c,b,a,e,k,j,i,m)   !cbakji (+1.000)
             enddo;enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       v3d=v3d+z9
!       deallocate(z9)
       deallocate(t4d)                                              !ilias: if no quadruples comment out the following 2 lines
       deallocate(x9)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      endif
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n2,n3,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s13(n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3))
       i1=k4*k2*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s13)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x13(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       x13=0.0d0
       call sum4123(n0,n2,n0,n2,n2,n3,n0,n2,x13,s13, 1.000)
c
       call sumx2143(n0,n3,n0,n2,n0,n2,n2,n3,n0,n2,x13,intb, 1.000)
c
      if(lvlq.eq.'ccsdtq'.or.lvlq.eq.'4cc')then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4e(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       rewind(te)
       read(te)t4e
!       allocate(h2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2))
!       call reorder56123478(n2,n3,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t4e,h2)
!       allocate(z13(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
!       i1=k2
!       i2=k2*k2*k4*k4*k4
!       i3=k4*k2*k2
!       call egemm(i1,i2,i3,x13,h2,z13)
!       deallocate(h2)
!       deallocate(t4e)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2                     !ilias: if no quadruples comment out the following 14 lines
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n2;do n=n0+1,n2
             sum=sum                                 !top 2 switched
     &     + (x13(n,m,e,j)*t4e(e,c,b,a,n,m,k,i)      !jcbaki(+0.500)
     &     - x13(n,m,e,i)*t4e(e,c,b,a,n,m,k,j)       !icbakj(-0.500)
     &     - x13(n,m,e,k)*t4e(e,c,b,a,n,m,j,i))/2.0d0!kcbaji(-0.500)
             enddo;enddo;enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       v3d=v3d-0.500*z13
!       call
!     & sum123465(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z13, 0.500)
!       call
!     & sum123564(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z13,-0.500)
!       deallocate(z13)
       deallocate(t4e)                                              !ilias: if no quadruples comment out the following 2 lines
       deallocate(x13)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      endif
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder2431(n0,n2,n0,n2,n0,n2,n2,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,s13,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s45(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s45)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder3124(n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n0,n2,n0,n2,s45,d1)
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z77(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,d1,d2,z77)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     - s45(b,k,m,i)*t2c(c,a,m,j)     !bkicaj (-1.000)
     &     - s45(c,j,m,i)*t2c(b,a,m,k)     !cjibak (-1.000)
     &     + s45(c,k,m,i)*t2c(b,a,m,j)     !ckibaj (+1.000)
     &     + s45(b,j,m,i)*t2c(c,a,m,k)     !bjicak (+1.000)
     &     + s45(b,k,m,j)*t2c(c,a,m,i)     !bkjcai (+1.000)
     &     + s45(c,i,m,j)*t2c(b,a,m,k)     !cijbak (+1.000)
     &     - s45(c,k,m,j)*t2c(b,a,m,i)     !ckjbai (-1.000)
     &     - s45(b,i,m,j)*t2c(c,a,m,k)     !bijcak (-1.000)
     &     - s45(b,j,m,k)*t2c(c,a,m,i)     !bjkcai (-1.000)
     &     - s45(c,i,m,k)*t2c(b,a,m,j)     !cikbaj (-1.000)
     &     + s45(c,j,m,k)*t2c(b,a,m,i)     !cjkbai (+1.000)
     &     + s45(b,i,m,k)*t2c(c,a,m,j)     !bikcaj (+1.000)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum135246(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z77,-1.000)
!       call
!     & sum234156(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z77,-1.000)
!       call
!     & sum235146(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z77, 1.000)
!       call
!     & sum134256(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z77, 1.000)
!       call
!     & sum136245(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z77, 1.000)
!       call
!     & sum234165(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z77, 1.000)
!       call
!     & sum236145(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z77,-1.000)
!       call
!     & sum134265(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z77,-1.000)
!       call
!     & sum136254(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z77,-1.000)
!       call
!     & sum235164(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z77,-1.000)
!       call
!     & sum236154(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z77, 1.000)
!       call
!     & sum135264(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z77, 1.000)
!       deallocate(z77)
       deallocate(s45)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder3421(n0,n2,n0,n2,n0,n2,n2,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,s13,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s44(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s44)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder3124(n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n0,n2,n0,n2,s44,d1)
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z76(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,d1,d2,z76)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - s44(a,k,n,i)*t2c(c,b,n,j)     !akicbj (-1.000)
     &     + s44(a,j,n,i)*t2c(c,b,n,k)     !ajicbk (+1.000)
     &     + s44(a,k,n,j)*t2c(c,b,n,i)     !akjcbi (+1.000)
     &     - s44(a,i,n,j)*t2c(c,b,n,k)     !aijcbk (-1.000)
     &     - s44(a,j,n,k)*t2c(c,b,n,i)     !ajkcbi (-1.000)
     &     + s44(a,i,n,k)*t2c(c,b,n,j)     !aikcbj (+1.000)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum125346(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z76,-1.000)
!       call
!     & sum124356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z76, 1.000)
!       call
!     & sum126345(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z76, 1.000)
!       call
!     & sum124365(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z76,-1.000)
!       call
!     & sum126354(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z76,-1.000)
!       call
!     & sum125364(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z76, 1.000)
!       deallocate(z76)
       deallocate(s44)
c
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder2341(n0,n2,n0,n2,n0,n2,n2,n3,
     & n0,n2,n0,n2,n2,n3,n0,n2,s13,d1)
       allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3412(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
       allocate(s43(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k4*k4
       i3=k2*k2
       call egemm(i1,i2,i3,d1,d2,s43)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n2,n3,n0,n2,
!     & n2,n3,n2,n3,n2,n3,n0,n2,s43,d1)
!       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
!       allocate(z75(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k2*k2*k4
!       i3=k4
!       call egemm(i1,i2,i3,d1,d2,z75)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum                 !top 2 switched
     &     + (s43(c,a,f,i)*t2c(f,b,k,j)      !caibkj (+0.500)
     &     - s43(b,a,f,i)*t2c(f,c,k,j)       !baickj (-0.500)
     &     - s43(c,b,f,i)*t2c(f,a,k,j)       !cbiakj (-0.500)
     &     + s43(b,a,f,j)*t2c(f,c,k,i)       !bajcki (+0.500)
     &     - s43(c,a,f,j)*t2c(f,b,k,i)       !cajbki (-0.500)
     &     + s43(c,b,f,j)*t2c(f,a,k,i)       !cbjaki (+0.500)
     &     - s43(b,a,f,k)*t2c(f,c,j,i)       !bakcji (-0.500)
     &     + s43(c,a,f,k)*t2c(f,b,j,i)       !cakbji (+0.500)
     &     - s43(c,b,f,k)*t2c(f,a,j,i))/2.0d0!cbkaji (-0.500)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum145236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z75,-0.500)
!       call
!     & sum245136(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z75, 0.500)
!       call
!     & sum345126(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z75,-0.500)
!       call
!     & sum146235(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z75, 0.500)
!       call
!     & sum246135(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z75,-0.500)
!       call
!     & sum346125(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z75, 0.500)
!       call
!     & sum156234(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z75,-0.500)
!       call
!     & sum256134(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z75, 0.500)
!       call
!     & sum356124(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z75,-0.500)
!       deallocate(z75)
       deallocate(s43)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder3421(n0,n2,n0,n2,n0,n2,n2,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,s13,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q17(n0+1:n2,n0+1:n2))
       i1=k2*k2
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q17)
       deallocate(d1)
       deallocate(b2)
c
       x4=x4-q17
       deallocate(q17)
c
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3241(n0,n2,n0,n2,n0,n2,n2,n3,
     & n0,n2,n0,n2,n2,n3,n0,n2,s13,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s39(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s39)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n2,n2,n3,n2,n3,n0,n2,x7,s39,-1.000)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder2314(n2,n3,n0,n2,n2,n3,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,s39,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s51(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s51)
       deallocate(d1)
       deallocate(b2)
       deallocate(s39)
c
       call sum2134(n2,n3,n2,n3,n2,n3,n0,n2,x2,s51, 1.000)
       deallocate(s51)
c
       call sumx3241(n0,n3,n2,n3,n2,n3,n2,n3,n0,n2,x2,intb, 1.000)
c
!       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
!       allocate(z2(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k2*k2*k4
!       i3=k4
!       call egemm(i1,i2,i3,x2,d2,z2)
!       deallocate(d2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     - x2(e,c,b,i)*t2c(e,a,k,j)     !cbiakj (-1.000)
     &     + x2(e,c,a,i)*t2c(e,b,k,j)     !caibkj (+1.000)
     &     - x2(e,b,a,i)*t2c(e,c,k,j)     !baickj (-1.000)
     &     + x2(e,c,b,j)*t2c(e,a,k,i)     !cbjaki (+1.000)
     &     - x2(e,c,a,j)*t2c(e,b,k,i)     !cajbki (-1.000)
     &     + x2(e,b,a,j)*t2c(e,c,k,i)     !bajcki (+1.000)
     &     - x2(e,c,b,k)*t2c(e,a,j,i)     !cbkaji (-1.000)
     &     + x2(e,c,a,k)*t2c(e,b,j,i)     !cakbji (+1.000)
     &     - x2(e,b,a,k)*t2c(e,c,j,i)     !bakcji (-1.000)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum345126(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z2,-1.000)
!       call
!     & sum245136(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z2, 1.000)
!       call
!     & sum145236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z2,-1.000)
!       call
!     & sum346125(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z2, 1.000)
!       call
!     & sum246135(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z2,-1.000)
!       call
!     & sum146235(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z2, 1.000)
!       call
!     & sum356124(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z2,-1.000)
!       call
!     & sum256134(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z2, 1.000)
!       call
!     & sum156234(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z2,-1.000)
!       deallocate(z2)
       deallocate(x2)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder4231(n0,n2,n0,n2,n0,n2,n2,n3,
     & n2,n3,n0,n2,n0,n2,n0,n2,s13,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s38(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s38)
       deallocate(d1)
       deallocate(b2)
       deallocate(s13)
c
       allocate(x6(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       x6=0.0d0
       call sum3124(n0,n2,n0,n2,n0,n2,n0,n2,x6,s38, 1.000)
c
       allocate(d1(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder3214(n0,n2,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n0,n2,n0,n2,s38,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s50(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s50)
       deallocate(d1)
       deallocate(b2)
       deallocate(s38)
c
       call sum2134(n0,n2,n2,n3,n0,n2,n0,n2,x1,s50,-1.000)
       deallocate(s50)
c
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s14(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       i1=k4*k4*k2
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s14)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x14(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       x14=0.0d0
       call sum4123(n0,n2,n2,n3,n2,n3,n2,n3,x14,s14,-1.000)
c
       call sumx4132(n0,n3,n0,n2,n2,n3,n2,n3,n2,n3,x14,intb, 1.000)
c
      if(lvlq.eq.'ccsdtq'.or.lvlq.eq.'4cc')then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4e(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       rewind(te)
       read(te)t4e
!       allocate(h2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n2))
!       call reorder51234678(n2,n3,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,t4e,h2)
!       allocate(z14(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3))
!       i1=k4
!       i2=k2*k2*k2*k4*k4
!       i3=k4*k4*k2
!       call egemm(i1,i2,i3,x14,h2,z14)
!       deallocate(h2)
!       deallocate(t4e)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2                     !ilias: if no quadruples comment out the following 14 lines
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do f=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + (x14(m,f,e,c)*t4e(f,e,b,a,m,k,j,i)      !cbakji(+0.500)
     &     - x14(m,f,e,b)*t4e(f,e,c,a,m,k,j,i)       !bcakji(-0.500)
     &     + x14(m,f,e,a)*t4e(f,e,c,b,m,k,j,i))/2.0d0!acbkji(+0.500)
             enddo;enddo;enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum234561(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z14, 0.500)
!       call
!     & sum134562(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z14,-0.500)
!       call
!     & sum124563(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z14, 0.500)
!       deallocate(z14)
       deallocate(t4e)                                              !ilias: if no quadruples comment out the following 2 lines
       deallocate(x14)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      endif
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
       call reorder3421(n2,n3,n0,n2,n2,n3,n2,n3,
     & n2,n3,n2,n3,n0,n2,n2,n3,s14,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(s48(n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k2*k2
       i3=k4*k4
       call egemm(i1,i2,i3,d1,d2,s48)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n0,n2,n2,n3,n0,n2,n0,n2,x1,s48,-0.500)
       deallocate(s48)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder2341(n2,n3,n0,n2,n2,n3,n2,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,s14,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s47(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s47)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3412(n2,n3,n0,n2,n2,n3,n2,n3,
!     & n2,n3,n2,n3,n2,n3,n0,n2,s47,d1)
!       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
!       allocate(z79(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k2*k2*k4
!       i3=k4
!       call egemm(i1,i2,i3,d1,d2,z79)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     + s47(b,k,e,c)*t2c(e,a,j,i)     !bkcaji    (+1.000)
     &     - s47(c,k,e,b)*t2c(e,a,j,i)     !ckbaji    (-1.000)
     &     + s47(c,k,e,a)*t2c(e,b,j,i)     !ckabji    (+1.000)
     &     - s47(b,j,e,c)*t2c(e,a,k,i)     !bjcaki    (-1.000)
     &     + s47(c,j,e,b)*t2c(e,a,k,i)     !cjbaki    (+1.000)
     &     - s47(c,j,e,a)*t2c(e,b,k,i)     !cjabki    (-1.000)
     &     + s47(b,i,e,c)*t2c(e,a,k,j)     !bicakj    (+1.000)
     &     - s47(c,i,e,b)*t2c(e,a,k,j)     !cibakj    (-1.000)
     &     + s47(c,i,e,a)*t2c(e,b,k,j)     !ciabkj    (+1.000)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum356124(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z79, 1.000)
!       call
!     & sum356214(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z79,-1.000)
!       call
!     & sum256314(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z79, 1.000)
!       call
!     & sum346125(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z79,-1.000)
!       call
!     & sum346215(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z79, 1.000)
!       call
!     & sum246315(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z79,-1.000)
!       call
!     & sum345126(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z79, 1.000)
!       call
!     & sum345216(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z79,-1.000)
!       call
!     & sum245316(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z79, 1.000)
!       deallocate(z79)
       deallocate(s47)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder2431(n2,n3,n0,n2,n2,n3,n2,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,s14,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s46(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s46)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x17(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       x17=0.0d0
       call sum3412(n2,n3,n2,n3,n2,n3,n0,n2,x17,s46, 1.000)
       deallocate(s46)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder2431(n2,n3,n0,n2,n2,n3,n2,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,s14,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q18(n2+1:n3,n2+1:n3))
       i1=k4*k4
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q18)
       deallocate(d1)
       deallocate(b2)
c
       x5=x5+q18
       deallocate(q18)
c
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
       call reorder2341(n2,n3,n0,n2,n2,n3,n2,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,s14,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s40(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s40)
       deallocate(d1)
       deallocate(b2)
       deallocate(s14)
c
       allocate(x8(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       x8=0.0d0
       call sum3124(n2,n3,n2,n3,n2,n3,n2,n3,x8,s40, 1.000)
       deallocate(s40)
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
c
       call sumx12(0,n3,n0,n2,n2,n3,x12,fockb, 1.000)
c
      if(lvlq.eq.'ccsdtq'.or.lvlq.eq.'4cc')then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       allocate(t4e(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,                !ilias: if no quadruples comment out the following 4 lines
     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       rewind(te)
       read(te)t4e
!       allocate(h2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,
!     & n0+1:n2,n0+1:n2))
!       call reorder51234678(n2,n3,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,t4e,h2)
!       allocate(z12(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
!       i2=k2*k2*k2*k4*k4*k4
!       i3=k4*k2
!       call egemm2(i2,i3,x12,h2,z12)
!       deallocate(h2)
!       deallocate(t4e)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2                     !ilias: if no quadruples comment out the following 12 lines
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x12(m,e)*t4e(e,c,b,a,m,k,j,i)   !cbakji  (+1.000)
             enddo;enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       v3d=v3d+z12
!       deallocate(z12)
       deallocate(t4e)                                              !ilias: if no quadruples comment out the following 2 lines
       deallocate(x12)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      endif
c
       allocate(b1(n2+1:n3,n0+1:n2))
       call reorder21(n0,n2,n2,n3,
     & n2,n3,n0,n2,q10,b1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(s49(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,b1,d2,s49)
       deallocate(b1)
       deallocate(d2)
       deallocate(q10)
c
       call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x1,s49, 1.000)
       deallocate(s49)
c
      if(t2diag4.eq.0)goto 6001                                    !ilias: testing the particle version of 3cc
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(s15(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s15)
       deallocate(d1)
       deallocate(d2)
c
       factor=-t2diag4
       call sum2314(n0,n2,n2,n3,n0,n2,n0,n2,x15,s15,factor)
       deallocate(s15)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z20(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x15,d2,z20)
!       deallocate(d2)
c
6001   do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + x15(m,c,j,i)*t2c(b,a,m,k)     !cjibak    (+1.000)
     &     - x15(m,b,j,i)*t2c(c,a,m,k)     !bjicak    (-1.000)
     &     + x15(m,a,j,i)*t2c(c,b,m,k)     !ajicbk    (+1.000)
     &     - x15(m,c,k,i)*t2c(b,a,m,j)     !ckibaj    (-1.000)
     &     + x15(m,b,k,i)*t2c(c,a,m,j)     !bkicaj    (+1.000)
     &     - x15(m,a,k,i)*t2c(c,b,m,j)     !akicbj    (-1.000)
     &     - x15(m,c,i,j)*t2c(b,a,m,k)     !cijbak    (-1.000)
     &     + x15(m,b,i,j)*t2c(c,a,m,k)     !bijcak    (+1.000)
     &     - x15(m,a,i,j)*t2c(c,b,m,k)     !aijcbk    (-1.000)
     &     + x15(m,c,i,k)*t2c(b,a,m,j)     !cikbaj    (+1.000)
     &     - x15(m,b,i,k)*t2c(c,a,m,j)     !bikcaj    (-1.000)
     &     + x15(m,a,i,k)*t2c(c,b,m,j)     !aikcbj    (+1.000)
     &     + x15(m,c,k,j)*t2c(b,a,m,i)     !ckjbai    (+1.000)
     &     - x15(m,b,k,j)*t2c(c,a,m,i)     !bkjcai    (-1.000)
     &     + x15(m,a,k,j)*t2c(c,b,m,i)     !akjcbi    (+1.000)
     &     - x15(m,c,j,k)*t2c(b,a,m,i)     !cjkbai    (-1.000)
     &     + x15(m,b,j,k)*t2c(c,a,m,i)     !bjkcai    (+1.000)
     &     - x15(m,a,j,k)*t2c(c,b,m,i)     !ajkcbi    (-1.000)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum234156(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20, 1.000)
!       call
!     & sum134256(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20,-1.000)
!       call
!     & sum124356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20, 1.000)
!       call
!     & sum235146(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20,-1.000)
!       call
!     & sum135246(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20, 1.000)
!       call
!     & sum125346(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20,-1.000)
!       call
!     & sum234165(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20,-1.000)
!       call
!     & sum134265(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20, 1.000)
!       call
!     & sum124365(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20,-1.000)
!       call
!     & sum235164(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20, 1.000)
!       call
!     & sum135264(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20,-1.000)
!       call
!     & sum125364(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20, 1.000)
!       call
!     & sum236145(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20, 1.000)
!       call
!     & sum136245(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20,-1.000)
!       call
!     & sum126345(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20, 1.000)
!       call
!     & sum236154(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20,-1.000)
!       call
!     & sum136254(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20, 1.000)
!       call
!     & sum126354(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z20,-1.000)
!       deallocate(z20)
       deallocate(x15)
c
      if(t2diag3.eq.0)goto 6002
       allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n2,n3,n2,n3,intm,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(s16(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s16)
       deallocate(d1)
       deallocate(d2)
c
       factor=t2diag3
       call sum3412(n2,n3,n2,n3,n2,n3,n0,n2,x16,s16,t2diag3)
       deallocate(s16)
c
!       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
!       allocate(z21(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k2*k2*k4
!       i3=k4
!       call egemm(i1,i2,i3,x16,d2,z21)
!       deallocate(d2)
c
6002   do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     - x16(e,c,b,i)*t2c(e,a,k,j)     !cbiakj    (-1.000)
     &     + x16(e,c,a,i)*t2c(e,b,k,j)     !caibkj    (+1.000)
     &     + x16(e,b,c,i)*t2c(e,a,k,j)     !bciakj    (+1.000)
     &     - x16(e,a,c,i)*t2c(e,b,k,j)     !acibkj    (-1.000)
     &     - x16(e,b,a,i)*t2c(e,c,k,j)     !baickj    (-1.000)
     &     + x16(e,a,b,i)*t2c(e,c,k,j)     !abickj    (+1.000)
     &     + x16(e,c,b,j)*t2c(e,a,k,i)     !cbjaki    (+1.000)
     &     - x16(e,c,a,j)*t2c(e,b,k,i)     !cajbki    (-1.000)
     &     - x16(e,b,c,j)*t2c(e,a,k,i)     !bcjaki    (-1.000)
     &     + x16(e,a,c,j)*t2c(e,b,k,i)     !acjbki    (+1.000)
     &     + x16(e,b,a,j)*t2c(e,c,k,i)     !bajcki    (+1.000)
     &     - x16(e,a,b,j)*t2c(e,c,k,i)     !abjcki    (-1.000)
     &     - x16(e,c,b,k)*t2c(e,a,j,i)     !cbkaji    (-1.000)
     &     + x16(e,c,a,k)*t2c(e,b,j,i)     !cakbji    (+1.000)
     &     + x16(e,b,c,k)*t2c(e,a,j,i)     !bckaji    (+1.000)
     &     - x16(e,a,c,k)*t2c(e,b,j,i)     !ackbji    (-1.000)
     &     - x16(e,b,a,k)*t2c(e,c,j,i)     !bakcji    (-1.000)
     &     + x16(e,a,b,k)*t2c(e,c,j,i)     !abkcji    (+1.000)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum345126(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21,-1.000)
!       call
!     & sum245136(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21, 1.000)
!       call
!     & sum345216(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21, 1.000)
!       call
!     & sum245316(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21,-1.000)
!       call
!     & sum145236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21,-1.000)
!       call
!     & sum145326(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21, 1.000)
!       call
!     & sum346125(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21, 1.000)
!       call
!     & sum246135(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21,-1.000)
!       call
!     & sum346215(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21,-1.000)
!       call
!     & sum246315(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21, 1.000)
!       call
!     & sum146235(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21, 1.000)
!       call
!     & sum146325(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21,-1.000)
!       call
!     & sum356124(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21,-1.000)
!       call
!     & sum256134(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21, 1.000)
!       call
!     & sum356214(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21, 1.000)
!       call
!     & sum256314(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21,-1.000)
!       call
!     & sum156234(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21,-1.000)
!       call
!     & sum156324(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z21, 1.000)
!       deallocate(z21)
       deallocate(x16)
c
      if(t3diag1.eq.0)goto 5001
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
      if(t3diag2.eq.0)then
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intm,d1)
      else
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n1,n3,intr,d1)
      endif
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(s17(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s17)
       deallocate(d1)
       deallocate(d2)
c
       factor=t3diag1
       call sum3412(n0,n1,n1,n3,n2,n3,n0,n2,x3,s17,factor)
       deallocate(s17)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n2,n3,intm,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(s18(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s18)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n0,n2,n2,n3,n2,n3,n0,n2,x7,s18,factor)
       deallocate(s18)
c
5001  if(t3diag4.eq.0)goto 5002
       allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(q11(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k3*k4*k1
       call egemm(i1,i2,i3,d1,d2,q11)
       deallocate(d1)
       deallocate(d2)
c
       factor=t3diag4
       call sum21(n0,n2,n0,n2,x4,q11,factor)
       deallocate(q11)
c
5002  if(t3diag3.eq.0)goto 5003
       allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n1,n3,n2,n3,intm,d1)
       allocate(d2(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))
       call reorder3421(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n0,n1,n1,n3,n2,n3,t2b,d2)
       allocate(q12(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k3*k1*k2
       call egemm(i1,i2,i3,d1,d2,q12)
       deallocate(d1)
       deallocate(d2)
c
       factor=-t3diag3
       call sum21(n2,n3,n2,n3,x5,q12,factor)
       deallocate(q12)
c
5003   allocate(b1(n2+1:n3,n0+1:n2))
       call reorder12(0,n3,0,n3,
     & n2,n3,n0,n2,fockb,b1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(s19(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2*k2*k4
       i3=k4
       call egemm(i1,i2,i3,b1,d2,s19)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x1,s19, 1.000)
       deallocate(s19)
c
      if(t2diag5.eq.0)goto 6003
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3412(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
       allocate(s20(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k4*k4
       i3=k2*k2
       call egemm(i1,i2,i3,d1,d2,s20)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n2,n3,n0,n2,
!     & n2,n3,n2,n3,n2,n3,n0,n2,s20,d1)
!       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
!       allocate(z46(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k2*k2*k4
!       i3=k4
!       call egemm(i1,i2,i3,d1,d2,z46)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum           !top 2 swtiched
     &     + (s20(c,a,e,i)*t2c(e,b,k,j)      !caibkj (+0.500)
     &     - s20(b,a,e,i)*t2c(e,c,k,j)       !baickj (-0.500)
     &     - s20(c,b,e,i)*t2c(e,a,k,j)       !cbiakj (-0.500)
     &     + s20(b,a,e,j)*t2c(e,c,k,i)       !bajcki (+0.500)
     &     - s20(c,a,e,j)*t2c(e,b,k,i)       !cajbki (-0.500)
     &     + s20(c,b,e,j)*t2c(e,a,k,i)       !cbjaki (+0.500)
     &     - s20(b,a,e,k)*t2c(e,c,j,i)       !bakcji (-0.500)
     &     + s20(c,a,e,k)*t2c(e,b,j,i)       !cakbji (+0.500)
     &     - s20(c,b,e,k)*t2c(e,a,j,i))/2.0d0!cbkaji (-0.500)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+t2diag5*sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum145236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z46,-0.500)
!       call
!     & sum245136(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z46, 0.500)
!       call
!     & sum345126(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z46,-0.500)
!       call
!     & sum146235(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z46, 0.500)
!       call
!     & sum246135(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z46,-0.500)
!       call
!     & sum346125(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z46, 0.500)
!       call
!     & sum156234(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z46,-0.500)
!       call
!     & sum256134(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z46, 0.500)
!       call
!     & sum356124(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z46,-0.500)
!       deallocate(z46)
       deallocate(s20)
c
6003  if(t2diag4.eq.0)goto 6004                                    !ilias: testing the particle version of 3cc
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1423(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s21(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s21)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder3124(n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n0,n2,n0,n2,s21,d1)
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z47(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,d1,d2,z47)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - s21(a,k,n,i)*t2c(c,b,n,j)     !akicbj (-1.000)
     &     + s21(b,k,n,i)*t2c(c,a,n,j)     !bkicaj (+1.000)
     &     + s21(c,j,n,i)*t2c(b,a,n,k)     !cjibak (+1.000)
     &     - s21(c,k,n,i)*t2c(b,a,n,j)     !ckibaj (-1.000)
     &     - s21(b,j,n,i)*t2c(c,a,n,k)     !bjicak (-1.000)
     &     + s21(a,j,n,i)*t2c(c,b,n,k)     !ajicbk (+1.000)
     &     + s21(a,k,n,j)*t2c(c,b,n,i)     !akjcbi (+1.000)
     &     - s21(b,k,n,j)*t2c(c,a,n,i)     !bkjcai (-1.000)
     &     - s21(c,i,n,j)*t2c(b,a,n,k)     !cijbak (-1.000)
     &     + s21(c,k,n,j)*t2c(b,a,n,i)     !ckjbai (+1.000)
     &     + s21(b,i,n,j)*t2c(c,a,n,k)     !bijcak (+1.000)
     &     - s21(a,i,n,j)*t2c(c,b,n,k)     !aijcbk (-1.000)
     &     - s21(a,j,n,k)*t2c(c,b,n,i)     !ajkcbi (-1.000)
     &     + s21(b,j,n,k)*t2c(c,a,n,i)     !bjkcai (+1.000)
     &     + s21(c,i,n,k)*t2c(b,a,n,j)     !cikbaj (+1.000)
     &     - s21(c,j,n,k)*t2c(b,a,n,i)     !cjkbai (-1.000)
     &     - s21(b,i,n,k)*t2c(c,a,n,j)     !bikcaj (-1.000)
     &     + s21(a,i,n,k)*t2c(c,b,n,j)     !aikcbj (+1.000)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+t2diag4*sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum125346(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47,-1.000)
!       call
!     & sum135246(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47, 1.000)
!       call
!     & sum234156(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47, 1.000)
!       call
!     & sum235146(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47,-1.000)
!       call
!     & sum134256(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47,-1.000)
!       call
!     & sum124356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47, 1.000)
!       call
!     & sum126345(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47, 1.000)
!       call
!     & sum136245(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47,-1.000)
!       call
!     & sum234165(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47,-1.000)
!       call
!     & sum236145(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47, 1.000)
!       call
!     & sum134265(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47, 1.000)
!       call
!     & sum124365(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47,-1.000)
!       call
!     & sum126354(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47,-1.000)
!       call
!     & sum136254(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47, 1.000)
!       call
!     & sum235164(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47, 1.000)
!       call
!     & sum236154(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47,-1.000)
!       call
!     & sum135264(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47,-1.000)
!       call
!     & sum125364(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z47, 1.000)
!       deallocate(z47)
       deallocate(s21)
c
6004  if(t2diag3.eq.0)goto 6005
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))                 !ilias: commented out 13 lines for 3cc
       call reorder2341(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n2,n3,intb,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s22(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s22)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3412(n2,n3,n0,n2,n2,n3,n2,n3,
!     & n2,n3,n2,n3,n2,n3,n0,n2,s22,d1)
!       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
!       allocate(z48(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k2*k2*k4
!       i3=k4
!       call egemm(i1,i2,i3,d1,d2,z48)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2                      !ilias: commented out 29 lines for 3cc
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum
     &     - s22(a,k,f,c)*t2c(f,b,j,i)     !akcbji (-1.000)
     &     + s22(b,k,f,c)*t2c(f,a,j,i)     !bkcaji (+1.000)
     &     + s22(a,k,f,b)*t2c(f,c,j,i)     !akbcji (+1.000)
     &     - s22(b,k,f,a)*t2c(f,c,j,i)     !bkacji (-1.000)
     &     - s22(c,k,f,b)*t2c(f,a,j,i)     !ckbaji (-1.000)
     &     + s22(c,k,f,a)*t2c(f,b,j,i)     !ckabji (+1.000)
     &     + s22(a,j,f,c)*t2c(f,b,k,i)     !ajcbki (+1.000)
     &     - s22(b,j,f,c)*t2c(f,a,k,i)     !bjcaki (-1.000)
     &     - s22(a,j,f,b)*t2c(f,c,k,i)     !ajbcki (-1.000)
     &     + s22(b,j,f,a)*t2c(f,c,k,i)     !bjacki (+1.000)
     &     + s22(c,j,f,b)*t2c(f,a,k,i)     !cjbaki (+1.000)
     &     - s22(c,j,f,a)*t2c(f,b,k,i)     !cjabki (-1.000)
     &     + s22(b,i,f,c)*t2c(f,a,k,j)     !bicakj (+1.000)
     &     - s22(a,i,f,c)*t2c(f,b,k,j)     !aicbkj (-1.000)
     &     - s22(c,i,f,b)*t2c(f,a,k,j)     !cibakj (-1.000)
     &     + s22(c,i,f,a)*t2c(f,b,k,j)     !ciabkj (+1.000)
     &     + s22(a,i,f,b)*t2c(f,c,k,j)     !aibckj (+1.000)
     &     - s22(b,i,f,a)*t2c(f,c,k,j)     !biackj (-1.000)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+t2diag3*sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum256134(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48,-1.000)
!       call
!     & sum356124(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48, 1.000)
!       call
!     & sum156234(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48, 1.000)
!       call
!     & sum156324(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48,-1.000)
!       call
!     & sum356214(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48,-1.000)
!       call
!     & sum256314(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48, 1.000)
!       call
!     & sum246135(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48, 1.000)
!       call
!     & sum346125(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48,-1.000)
!       call
!     & sum146235(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48,-1.000)
!       call
!     & sum146325(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48, 1.000)
!       call
!     & sum346215(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48, 1.000)
!       call
!     & sum246315(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48,-1.000)
!       call
!     & sum345126(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48, 1.000)
!       call
!     & sum245136(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48,-1.000)
!       call
!     & sum345216(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48,-1.000)
!       call
!     & sum245316(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48, 1.000)
!       call
!     & sum145236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48, 1.000)
!       call
!     & sum145326(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z48,-1.000)
!       deallocate(z48)
       deallocate(s22)                                               !ilias: commented out this line for 3cc
c
6005  if(t2diag5.eq.0)goto 6006
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n2,n3,intb,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(s23(n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k2*k2
       i3=k4*k4
       call egemm(i1,i2,i3,d1,d2,s23)
       deallocate(d1)
       deallocate(d2)
c
       factor=0.500*t2diag5
       call sum3412(n0,n2,n2,n3,n0,n2,n0,n2,x1,s23,factor)
       deallocate(s23)
c
6006   call sumx2143(n0,n3,n0,n2,n2,n3,n0,n2,n0,n2,x1,intb, 1.000)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z1(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x1,d2,z1)
!       deallocate(d2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + x1(m,c,j,i)*t2c(b,a,m,k)     !cjibak (+1.000)
     &     - x1(m,b,j,i)*t2c(c,a,m,k)     !bjicak (-1.000)
     &     + x1(m,a,j,i)*t2c(c,b,m,k)     !ajicbk (+1.000)
     &     - x1(m,c,k,i)*t2c(b,a,m,j)     !ckibaj (-1.000)
     &     + x1(m,b,k,i)*t2c(c,a,m,j)     !bkicaj (+1.000)
     &     - x1(m,a,k,i)*t2c(c,b,m,j)     !akicbj (-1.000)
     &     + x1(m,c,k,j)*t2c(b,a,m,i)     !ckjbai (+1.000)
     &     - x1(m,b,k,j)*t2c(c,a,m,i)     !bkjcai (-1.000)
     &     + x1(m,a,k,j)*t2c(c,b,m,i)     !akjcbi (+1.000)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum234156(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z1, 1.000)
!       call
!     & sum134256(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z1,-1.000)
!       call
!     & sum124356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z1, 1.000)
!       call
!     & sum235146(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z1,-1.000)
!       call
!     & sum135246(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z1, 1.000)
!       call
!     & sum125346(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z1,-1.000)
!       call
!     & sum236145(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z1, 1.000)
!       call
!     & sum136245(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z1,-1.000)
!       call
!     & sum126345(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z1, 1.000)
!       deallocate(z1)
       deallocate(x1)
c
       allocate(x999(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       x999=0.0d0
c
      if(t3diag3.eq.0)goto 5004
       allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))                 !ilias: commented out 13 lines for 3cc
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n1,n3,n2,n3,intm,d1)
       allocate(f2(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder463125(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n2,n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,t3c,f2)
       allocate(s24(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
       i1=k4
       i2=k2*k4*k4
       i3=k3*k1*k2
       call egemm(i1,i2,i3,d1,f2,s24)
       deallocate(d1)
       deallocate(f2)
c
!       call sum2341(n2,n3,n2,n3,n2,n3,n0,n2,x17,s24, 1.000)
!       deallocate(s24)
!npb
      factor=t3diag3
       call sum2341(n2,n3,n2,n3,n2,n3,n0,n2,x999,s24,factor)         !ilias: commented out 2 lines for 3cc
       deallocate(s24)
!       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
!       allocate(z999(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k2*k2*k4
!       i3=k4
!       call egemm(i1,i2,i3,x999,d2,z999)
!       deallocate(d2)
c
5004   do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum                     !(bc)
     &     + x999(f,b,a,k)*t2c(f,c,j,i)     !bakcji    (+1.000)
     &     - x999(f,c,a,k)*t2c(f,b,j,i)     !cakbji    (-1.000)
     &     + x999(f,c,b,k)*t2c(f,a,j,i)     !cbkaji    (+1.000)
     &     - x999(f,b,a,j)*t2c(f,c,k,i)     !bajcki    (-1.000)
     &     + x999(f,c,a,j)*t2c(f,b,k,i)     !cajbki    (+1.000)
     &     - x999(f,c,b,j)*t2c(f,a,k,i)     !cbjaki    (-1.000)
     &     + x999(f,b,a,i)*t2c(f,c,k,j)     !baickj    (+1.000)
     &     - x999(f,c,a,i)*t2c(f,b,k,j)     !caibkj    (-1.000)
     &     + x999(f,c,b,i)*t2c(f,a,k,j)     !cbiakj    (+1.000)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!!      call sum256134(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999, 1.000)
!!      call sum156234(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999,-1.000)
!!      call sum156324(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999, 1.000)
!!      call sum246135(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999,-1.000)
!!      call sum146235(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999, 1.000)
!!      call sum146325(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999,-1.000)
!!      call sum245136(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999, 1.000)
!!      call sum145236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999,-1.000)
!!      call sum145326(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999, 1.000)
!!(bc)
!      call sum156234(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999, 1.000)
!      call sum256134(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999,-1.000)
!      call sum356124(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999, 1.000)
!      call sum146235(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999,-1.000)
!      call sum246135(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999, 1.000)
!      call sum346125(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999,-1.000)
!      call sum145236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999, 1.000)
!      call sum245136(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999,-1.000)
!      call sum345126(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z999, 1.000)
!       deallocate(z999)
       deallocate(x999)
!npb
c
!npb the following can be moved to the last occurace of x17 which happens 
!to be the only occurance of x17, and therefore x17 can just be replace with 
!s43 which is being added
!       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
!       allocate(z78(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k2*k2*k4
!       i3=k4
!       call egemm(i1,i2,i3,x17,d2,z78)
!       deallocate(d2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum
     &     + x17(f,c,a,k)*t2c(f,b,j,i)     !cakbji (+1.000)
     &     - x17(f,b,a,k)*t2c(f,c,j,i)     !bakcji (-1.000)
     &     + x17(f,a,b,k)*t2c(f,c,j,i)     !abkcji (+1.000)
     &     - x17(f,c,a,j)*t2c(f,b,k,i)     !cajbki (-1.000)
     &     + x17(f,b,a,j)*t2c(f,c,k,i)     !bajcki (+1.000)
     &     - x17(f,a,b,j)*t2c(f,c,k,i)     !abjcki (-1.000)
     &     + x17(f,c,a,i)*t2c(f,b,k,j)     !caibkj (+1.000)
     &     - x17(f,b,a,i)*t2c(f,c,k,j)     !baickj (-1.000)
     &     + x17(f,a,b,i)*t2c(f,c,k,j)     !abickj (+1.000)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum256134(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z78, 1.000)
!       call
!     & sum156234(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z78,-1.000)
!       call
!     & sum156324(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z78, 1.000)
!       call
!     & sum246135(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z78,-1.000)
!       call
!     & sum146235(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z78, 1.000)
!       call
!     & sum146325(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z78,-1.000)
!       call
!     & sum245136(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z78, 1.000)
!       call
!     & sum145236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z78,-1.000)
!       call
!     & sum145326(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z78, 1.000)
!       deallocate(z78)
       deallocate(x17)
c
      if(t3diag4.eq.0)goto 5005
       allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder613245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
       allocate(s25(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2*k2*k4
       i3=k3*k4*k1
       call egemm(i1,i2,i3,d1,f2,s25)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder4123(n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n0,n2,n0,n2,s25,d1)
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z51(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,d1,d2,z51)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     + s25(a,k,j,n)*t2c(c,b,n,i)     !akjcbi (+1.000)
     &     - s25(b,k,j,n)*t2c(c,a,n,i)     !bkjcai (-1.000)
     &     + s25(c,k,j,n)*t2c(b,a,n,i)     !ckjbai (+1.000)
     &     - s25(a,k,i,n)*t2c(c,b,n,j)     !akicbj (-1.000)
     &     + s25(b,k,i,n)*t2c(c,a,n,j)     !bkicaj (+1.000)
     &     - s25(c,k,i,n)*t2c(b,a,n,j)     !ckibaj (-1.000)
     &     + s25(a,j,i,n)*t2c(c,b,n,k)     !ajicbk (+1.000)
     &     - s25(b,j,i,n)*t2c(c,a,n,k)     !bjicak (-1.000)
     &     + s25(c,j,i,n)*t2c(b,a,n,k)     !cjibak (+1.000)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+t3diag4*sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum126345(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z51, 1.000)
!       call
!     & sum136245(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z51,-1.000)
!       call
!     & sum236145(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z51, 1.000)
!       call
!     & sum125346(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z51,-1.000)
!       call
!     & sum135246(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z51, 1.000)
!       call
!     & sum235146(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z51,-1.000)
!       call
!     & sum124356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z51, 1.000)
!       call
!     & sum134256(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z51,-1.000)
!       call
!     & sum234156(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z51, 1.000)
!       deallocate(z51)
       deallocate(s25)
c
5005  if(t3diag1.eq.0)goto 5006
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n1+1:n3))                 !ilias: commented out 16 lines for 3cc
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n1,n1,n3,intm,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s26(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3))
       i1=k3*k1
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s26)
       deallocate(d1)
       deallocate(d2)
c
       factor=t3diag1
       call sum3412(n0,n1,n1,n3,n2,n3,n0,n2,x3,s26,factor)
       deallocate(s26)
c
5006   call sumx1324(n0,n3,n0,n1,n1,n3,n2,n3,n0,n2,x3,intm, 1.000)
c
!       allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder631245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
!       allocate(z3(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
!       i1=k2*k4
!       i2=k2*k2*k4*k4
!       i3=k3*k1
!       call egemm(i1,i2,i3,x3,f2,z3)
!       deallocate(f2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + x3(m,e,c,i)*t3c(b,a,e,k,j,m)   !cibakj  (+1.000)
     &     - x3(m,e,b,i)*t3c(c,a,e,k,j,m)   !bicakj  (-1.000)
     &     + x3(m,e,a,i)*t3c(c,b,e,k,j,m)   !aicbkj  (+1.000)
     &     - x3(m,e,c,j)*t3c(b,a,e,k,i,m)   !cjbaki  (-1.000)
     &     + x3(m,e,b,j)*t3c(c,a,e,k,i,m)   !bjcaki  (+1.000)
     &     - x3(m,e,a,j)*t3c(c,b,e,k,i,m)   !ajcbki  (-1.000)
     &     + x3(m,e,c,k)*t3c(b,a,e,j,i,m)   !ckbaji  (+1.000)
     &     - x3(m,e,b,k)*t3c(c,a,e,j,i,m)   !bkcaji  (-1.000)
     &     + x3(m,e,a,k)*t3c(c,b,e,j,i,m)   !akcbji  (+1.000)
             enddo;enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum234516(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z3, 1.000)
!       call
!     & sum134526(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z3,-1.000)
!       call
!     & sum124536(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z3, 1.000)
!       call
!     & sum234615(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z3,-1.000)
!       call
!     & sum134625(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z3, 1.000)
!       call
!     & sum124635(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z3,-1.000)
!       call
!     & sum235614(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z3, 1.000)
!       call
!     & sum135624(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z3,-1.000)
!       call
!     & sum125634(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z3, 1.000)
!       deallocate(z3)
       deallocate(x3)
c
      if(t3diag3.eq.0)goto 5007
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))                 !ilias: commented out 13 lines for 3cc
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder451236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,t3d,f2)
       allocate(s27(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
       i1=k4
       i2=k2*k4*k4
       i3=k4*k2*k2
       call egemm(i1,i2,i3,d1,f2,s27)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n2,n3,n0,n2,n2,n3,
!     & n2,n3,n2,n3,n2,n3,n0,n2,s27,d1)
!       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
!       allocate(z53(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       i1=k2*k4*k4
!       i2=k2*k2*k4
!       i3=k4
!       call egemm(i1,i2,i3,d1,d2,z53)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2                      !ilias: commented out 20 lines for 3cc
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     + (s27(b,a,k,e)*t2c(e,c,j,i)      !bakcji (+0.500)
     &     - s27(c,a,k,e)*t2c(e,b,j,i)       !cakbji (-0.500)
     &     + s27(c,b,k,e)*t2c(e,a,j,i)       !cbkaji (+0.500)
     &     - s27(b,a,j,e)*t2c(e,c,k,i)       !bajcki (-0.500)
     &     + s27(c,a,j,e)*t2c(e,b,k,i)       !cajbki (+0.500)
     &     - s27(c,b,j,e)*t2c(e,a,k,i)       !cbjaki (-0.500)
     &     + s27(b,a,i,e)*t2c(e,c,k,j)       !baickj (+0.500)
     &     - s27(c,a,i,e)*t2c(e,b,k,j)       !caibkj (-0.500)
     &     + s27(c,b,i,e)*t2c(e,a,k,j))/2.0d0!cbiakj (+0.500)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+t3diag3*sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum156234(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z53, 0.500)
!       call
!     & sum256134(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z53,-0.500)
!       call
!     & sum356124(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z53, 0.500)
!       call
!     & sum146235(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z53,-0.500)
!       call
!     & sum246135(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z53, 0.500)
!       call
!     & sum346125(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z53,-0.500)
!       call
!     & sum145236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z53, 0.500)
!       call
!     & sum245136(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z53,-0.500)
!       call
!     & sum345126(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z53, 0.500)
!       deallocate(z53)
       deallocate(s27)                                               !ilias: commented out this line for 3cc
c
5007  if(t3diag5.eq.0)goto 5008
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(s28(n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k2*k2
       i3=k4*k4
       call egemm(i1,i2,i3,d1,d2,s28)
       deallocate(d1)
       deallocate(d2)
c
       factor=0.500*t3diag5
       call sum3412(n0,n2,n0,n2,n0,n2,n0,n2,x6,s28,factor)
       deallocate(s28)
c
5008   call sumx2143(n0,n3,n0,n2,n0,n2,n0,n2,n0,n2,x6,intb, 1.000)
c
!       allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder451236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,t3d,f2)
!       allocate(z6(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
!       i1=k2*k2
!       i2=k2*k4*k4*k4
!       i3=k2*k2
!       call egemm(i1,i2,i3,x6,f2,z6)
!       deallocate(f2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2;do n=n0+1,n2
             sum=sum
     &     + (x6(n,m,j,i)*t3d(c,b,a,n,m,k)      !jicbak (+0.500)
     &     - x6(n,m,k,i)*t3d(c,b,a,n,m,j)       !kicbaj (-0.500)
     &     + x6(n,m,k,j)*t3d(c,b,a,n,m,i))/2.0d0!kjcbai (+0.500)
             enddo;enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       v3d=v3d+0.500*z6
!       call
!     & sum123546(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z6,-0.500)
!       call
!     & sum123645(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z6, 0.500)
!       deallocate(z6)
       deallocate(x6)
c
      if(t3diag4.eq.0)goto 5009
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder412356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
       allocate(s29(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2*k2*k4
       i3=k4*k4*k2
       call egemm(i1,i2,i3,d1,f2,s29)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder4123(n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n0,n2,n0,n2,s29,d1)
!       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
!       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
!       allocate(z55(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
!       i1=k2*k2*k4
!       i2=k2*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,d1,d2,z55)
!       deallocate(d1)
!       deallocate(d2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + (s29(a,k,j,m)*t2c(c,b,m,i)      !akjcbi (+0.500)
     &     - s29(b,k,j,m)*t2c(c,a,m,i)       !bkjcai (-0.500)
     &     + s29(c,k,j,m)*t2c(b,a,m,i)       !ckjbai (+0.500)
     &     - s29(a,k,i,m)*t2c(c,b,m,j)       !akicbj (-0.500)
     &     + s29(b,k,i,m)*t2c(c,a,m,j)       !bkicaj (+0.500)
     &     - s29(c,k,i,m)*t2c(b,a,m,j)       !ckibaj (-0.500)
     &     + s29(a,j,i,m)*t2c(c,b,m,k)       !ajicbk (+0.500)
     &     - s29(b,j,i,m)*t2c(c,a,m,k)       !bjicak (-0.500)
     &     + s29(c,j,i,m)*t2c(b,a,m,k))/2.0d0!cjibak (+0.500)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+t3diag4*sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum126345(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z55, 0.500)
!       call
!     & sum136245(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z55,-0.500)
!       call
!     & sum236145(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z55, 0.500)
!       call
!     & sum125346(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z55,-0.500)
!       call
!     & sum135246(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z55, 0.500)
!       call
!     & sum235146(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z55,-0.500)
!       call
!     & sum124356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z55, 0.500)
!       call
!     & sum134256(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z55,-0.500)
!       call
!     & sum234156(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z55, 0.500)
!       deallocate(z55)
       deallocate(s29)
c
5009  if(t3diag1.eq.0)goto 5010
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
      if(t3diag2.eq.0)then
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intm,d1)
      else
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n2,n3,intb,d1)
      endif
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(s30(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
       i1=k4*k2
       i2=k2*k4
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s30)
       deallocate(d1)
       deallocate(d2)
c
       factor=t3diag1
       call sum3412(n0,n2,n2,n3,n2,n3,n0,n2,x7,s30,factor)
       deallocate(s30)
c
5010   call sumx3142(n0,n3,n0,n2,n2,n3,n2,n3,n0,n2,x7,intb, 1.000)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder412356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
!       allocate(z7(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
!       i1=k2*k4
!       i2=k2*k2*k4*k4
!       i3=k4*k2
!       call egemm(i1,i2,i3,x7,f2,z7)
!       deallocate(f2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x7(m,e,c,i)*t3d(e,b,a,m,k,j)   !cibakj  (+1.000)
     &     - x7(m,e,b,i)*t3d(e,c,a,m,k,j)   !bicakj  (-1.000)
     &     + x7(m,e,a,i)*t3d(e,c,b,m,k,j)   !aicbkj  (+1.000)
     &     - x7(m,e,c,j)*t3d(e,b,a,m,k,i)   !cjbaki  (-1.000)
     &     + x7(m,e,b,j)*t3d(e,c,a,m,k,i)   !bjcaki  (+1.000)
     &     - x7(m,e,a,j)*t3d(e,c,b,m,k,i)   !ajcbki  (-1.000)
     &     + x7(m,e,c,k)*t3d(e,b,a,m,j,i)   !ckbaji  (+1.000)
     &     - x7(m,e,b,k)*t3d(e,c,a,m,j,i)   !bkcaji  (-1.000)
     &     + x7(m,e,a,k)*t3d(e,c,b,m,j,i)   !akcbji  (+1.000)
             enddo;enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum234516(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z7, 1.000)
!       call
!     & sum134526(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z7,-1.000)
!       call
!     & sum124536(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z7, 1.000)
!       call
!     & sum234615(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z7,-1.000)
!       call
!     & sum134625(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z7, 1.000)
!       call
!     & sum124635(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z7,-1.000)
!       call
!     & sum235614(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z7, 1.000)
!       call
!     & sum135624(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z7,-1.000)
!       call
!     & sum125634(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z7, 1.000)
!       deallocate(z7)
       deallocate(x7)
c
      if(t3diag4.eq.0)goto 5011
       allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3124(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n2,n3,n0,n2,t2c,d2)
       allocate(q13(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k4*k4*k2
       call egemm(i1,i2,i3,d1,d2,q13)
       deallocate(d1)
       deallocate(d2)
c
       factor=-0.500*t3diag4
       call sum21(n0,n2,n0,n2,x4,q13,factor)
       deallocate(q13)
c
5011   call sumx12(0,n3,n0,n2,n0,n2,x4,fockb, 1.000)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
!       call reorder412356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
!       allocate(z4(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
!       i1=k2
!       i2=k2*k2*k4*k4*k4
!       i3=k2
!       call egemm(i1,i2,i3,x4,f2,z4)
!       deallocate(f2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     - x4(m,i)*t3d(c,b,a,m,k,j)     !icbakj (-1.000)
     &     + x4(m,j)*t3d(c,b,a,m,k,i)     !jcbaki (+1.000)
     &     - x4(m,k)*t3d(c,b,a,m,j,i)     !kcbaji (-1.000)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       v3d=v3d-z4
!       call
!     & sum123465(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z4, 1.000)
!       call
!     & sum123564(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z4,-1.000)
!       deallocate(z4)
       deallocate(x4)
c
      if(t3diag5.eq.0)goto 5012
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3412(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
       allocate(s31(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k4*k4
       i3=k2*k2
       call egemm(i1,i2,i3,d1,d2,s31)
       deallocate(d1)
       deallocate(d2)
c
       factor=0.500*t3diag5
       call sum3412(n2,n3,n2,n3,n2,n3,n2,n3,x8,s31,factor)
       deallocate(s31)
c
5012   call sumx4321(n0,n3,n2,n3,n2,n3,n2,n3,n2,n3,x8,intb, 1.000)
c
!       allocate(f2(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
!       call reorder123456(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,t3d,f2)
!       allocate(z8(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
!       i1=k4*k4
!       i2=k2*k2*k2*k4
!       i3=k4*k4
!       call egemm(i1,i2,i3,x8,f2,z8)
!       deallocate(f2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do f=n2+1,n3
             sum=sum
     &     + (x8(f,e,c,b)*t3d(f,e,a,k,j,i)      !cbakji (+0.500)
     &     - x8(f,e,c,a)*t3d(f,e,b,k,j,i)       !cabkji (-0.500)
     &     + x8(f,e,b,a)*t3d(f,e,c,k,j,i))/2.0d0!backji (+0.500)
             enddo;enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum345612(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z8, 0.500)
!       call
!     & sum245613(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z8,-0.500)
!       call
!     & sum145623(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z8, 0.500)
!       deallocate(z8)
       deallocate(x8)
c
      if(t3diag3.eq.0)goto 5013
       allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))                 !ilias: commented out 16 lines for 3cc
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(d2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3412(n2,n3,n2,n3,n0,n2,n0,n2,
     & n0,n2,n0,n2,n2,n3,n2,n3,t2c,d2)
       allocate(q14(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k4*k2*k2
       call egemm(i1,i2,i3,d1,d2,q14)
       deallocate(d1)
       deallocate(d2)
c
       factor=0.500*t3diag3
       call sum21(n2,n3,n2,n3,x5,q14,factor)
       deallocate(q14)
c
5013   call sumx21(0,n3,n2,n3,n2,n3,x5,fockb, 1.000)
c
!       allocate(f2(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
!       call reorder123456(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,
!     & n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,t3d,f2)
!       allocate(z5(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3))
!       i1=k4
!       i2=k2*k2*k2*k4*k4
!       i3=k4
!       call egemm(i1,i2,i3,x5,f2,z5)
!       deallocate(f2)
c
       do i=n0+1,n2-2;do j=i+1,n2-1;do k=j+1,n2
        if(indocc(k,j,i).eq.1)cycle
          do a=n2+1,n3-2;do b=a+1,n3-1;do c=b+1,n3
          if(indunocc(c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     + x5(e,c)*t3d(e,b,a,k,j,i)     !cbakji (+1.000)
     &     - x5(e,b)*t3d(e,c,a,k,j,i)     !bcakji (-1.000)
     &     + x5(e,a)*t3d(e,c,b,k,j,i)     !acbkji (+1.000)
             enddo
             v3d(c,b,a,k,j,i)=v3d(c,b,a,k,j,i)+sum
          enddo;enddo;enddo
       enddo;enddo;enddo
c
!       call
!     & sum234561(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z5, 1.000)
!       call
!     & sum134562(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z5,-1.000)
!       call
!     & sum124563(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2,v3d,z5, 1.000)
!       deallocate(z5)
       deallocate(x5)
c
       do i=n0+1,n2-2
       do j=i+1,n2-1
       do k=j+1,n2
       do a=n2+1,n3-2
       do b=a+1,n3-1
       do c=b+1,n3
!
!        ioccb=0
!        iunob=0
!        if(i.gt.(n2-iactoccb))ioccb=ioccb+1
!        if(j.gt.(n2-iactoccb))ioccb=ioccb+1
!        if(k.gt.(n2-iactoccb))ioccb=ioccb+1
!        if(ioccb.lt.iactindt)cycle
!        if(a.lt.(n2+iactunob+1))iunob=iunob+1
!        if(b.lt.(n2+iactunob+1))iunob=iunob+1
!        if(c.lt.(n2+iactunob+1))iunob=iunob+1
!        if(iunob.lt.iactindt)cycle
!
         coeleft=fockb(c,c)
     &          +fockb(b,b)
     &          +fockb(a,a)
     &          -fockb(k,k)
     &          -fockb(j,j)
     &          -fockb(i,i)
     &          +shift
         t3d(c,b,a,k,j,i)=t3d(c,b,a,k,j,i)-v3d(c,b,a,k,j,i)/coeleft
         t3d(c,b,a,k,i,j)=-t3d(c,b,a,k,j,i)
         t3d(c,b,a,i,j,k)=-t3d(c,b,a,k,j,i)
         t3d(c,b,a,i,k,j)= t3d(c,b,a,k,j,i)
         t3d(c,b,a,j,k,i)=-t3d(c,b,a,k,j,i)
         t3d(c,b,a,j,i,k)= t3d(c,b,a,k,j,i)
         t3d(c,a,b,k,j,i)=-t3d(c,b,a,k,j,i)
         t3d(c,a,b,k,i,j)= t3d(c,b,a,k,j,i)
         t3d(c,a,b,i,j,k)= t3d(c,b,a,k,j,i)
         t3d(c,a,b,i,k,j)=-t3d(c,b,a,k,j,i)
         t3d(c,a,b,j,k,i)= t3d(c,b,a,k,j,i)
         t3d(c,a,b,j,i,k)=-t3d(c,b,a,k,j,i)
         t3d(a,b,c,k,j,i)=-t3d(c,b,a,k,j,i)
         t3d(a,b,c,k,i,j)= t3d(c,b,a,k,j,i)
         t3d(a,b,c,i,j,k)= t3d(c,b,a,k,j,i)
         t3d(a,b,c,i,k,j)=-t3d(c,b,a,k,j,i)
         t3d(a,b,c,j,k,i)= t3d(c,b,a,k,j,i)
         t3d(a,b,c,j,i,k)=-t3d(c,b,a,k,j,i)
         t3d(a,c,b,k,j,i)= t3d(c,b,a,k,j,i)
         t3d(a,c,b,k,i,j)=-t3d(c,b,a,k,j,i)
         t3d(a,c,b,i,j,k)=-t3d(c,b,a,k,j,i)
         t3d(a,c,b,i,k,j)= t3d(c,b,a,k,j,i)
         t3d(a,c,b,j,k,i)=-t3d(c,b,a,k,j,i)
         t3d(a,c,b,j,i,k)= t3d(c,b,a,k,j,i)
         t3d(b,c,a,k,j,i)=-t3d(c,b,a,k,j,i)
         t3d(b,c,a,k,i,j)= t3d(c,b,a,k,j,i)
         t3d(b,c,a,i,j,k)= t3d(c,b,a,k,j,i)
         t3d(b,c,a,i,k,j)=-t3d(c,b,a,k,j,i)
         t3d(b,c,a,j,k,i)= t3d(c,b,a,k,j,i)
         t3d(b,c,a,j,i,k)=-t3d(c,b,a,k,j,i)
         t3d(b,a,c,k,j,i)= t3d(c,b,a,k,j,i)
         t3d(b,a,c,k,i,j)=-t3d(c,b,a,k,j,i)
         t3d(b,a,c,i,j,k)=-t3d(c,b,a,k,j,i)
         t3d(b,a,c,i,k,j)= t3d(c,b,a,k,j,i)
         t3d(b,a,c,j,k,i)=-t3d(c,b,a,k,j,i)
         t3d(b,a,c,j,i,k)= t3d(c,b,a,k,j,i)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
c
       end
